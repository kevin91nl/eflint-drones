{-# LANGUAGE OverloadedStrings, LambdaCase #-}

import Spec hiding (Value(..))
import State
import Util 
import Options
import StaticEval
import Parse
import Print (ppPhrase)
import Sim (make_initial_state)
import NewExplorer hiding (Instruction(Revert), Response)
import Interpreter
import JSON(decode_json_file)
import qualified NewExplorer as Explorer
import qualified Language.Explorer.Pure as EI

import Control.Monad
import Control.Applicative

import Data.Function (on)
import Data.Text (unpack)
import Text.Read (readMaybe)
import Data.List (isSuffixOf, sortBy, (\\))
import qualified Data.Set as S

import System.IO (hGetLine, hPutStrLn, hClose, IOMode(ReadWriteMode))
import System.IO.Error
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import Network.Socket 
import qualified Data.ByteString.Lazy.Char8 (pack,unpack)

import Data.Aeson hiding (String, Value(..),Options(..))
import qualified Data.Aeson as JSON

-- determines which variant of execution graph to use
init_explorer = init_stack_explorer

main :: IO ()
main = do
  args <- getArgs 
  cdir <- getCurrentDirectory 
  case args of 
    (f:p:opts) | ".eflint" `isSuffixOf` f, Just port_nr <- readMaybe p ->  do 
                  fcont <- readFile f
                  opts' <- run_options (["-i",takeDirectory f,"-i",cdir] ++ opts)
                  case parse_component syn_directives_phrases fcont of
                    Left err1 -> case parse_flint fcont of
                      Left err2 -> do  putStrLn "could not parse flint phrases:\n" >> putStrLn err1
                                       putStrLn "could not parse flint spec:\n" >> putStrLn err2
                      Right (s, r, i, _) -> init_server opts' port_nr s r i 
                    Right ps -> init_with_phrases opts' port_nr ps 
    (p:opts)   | Just port_nr <- readMaybe p -> do 
      opts <- run_options (["-i",cdir] ++ opts)
      init_server opts port_nr emptySpec emptyRefiner emptyInitialiser  
    _ -> putStrLn "please provide: <NAME>.eflint <PORT> <OPTIONS>"

init_with_phrases :: Options -> PortNumber -> [Either Directive Phrase] -> IO ()
init_with_phrases opts port_nr ps = do
  let explorer = init_explorer emptySpec emptyState
  run_directives_phrases opts ps (start_server opts port_nr) explorer
  return ()

type Cont a = Explorer -> IO a

run_directives_phrases :: Options -> [Either Directive Phrase] -> Cont a -> Cont a
run_directives_phrases opts [] cont exp = cont exp
run_directives_phrases opts (edp:ps) cont exp = let (_,_,(_,ctx)) = get_last_edge exp (EI.currRef exp) in 
  case edp of 
   Left d  -> run_directive opts d (run_directives_phrases opts ps cont) exp
   Right p -> run_phrase opts p (run_directives_phrases opts ps cont) exp

run_phrase :: Options -> Phrase -> Cont a -> Cont a
run_phrase opts phrase cont exp = case run_ exp (Execute [phrase]) of
  ResultTrans exp _ _ _  -> cont exp
  Path _                 -> putStrLn "Unexpected execution path encountered" >> cont exp 
  Nodes _                -> putStrLn "Unexpected collection of nodes encountered" >> cont exp 
  InvalidRevert          -> putStrLn "invalid revert error" >> cont exp

run_directive :: Options -> Directive -> Cont a -> Cont a
run_directive opts (Require fp) cont exp 
  | has_been_included fp opts = cont exp
  | otherwise                 = run_directive opts (Include fp) cont exp
run_directive opts (Include fp) cont exp = do
  let dirs = find include_paths opts
  find_included_file dirs fp >>= \case 
    []       -> putStrLn ("could not find " ++ fp ++ " in " ++ show dirs) >> cont exp
    (file:_) -> do
      add_include_path (takeDirectory file) opts
      add_include file opts
      case ".json" `isSuffixOf` file of
        True -> do
          mspec <- decode_json_file file 
          case mspec of
            Left err -> putStrLn err >> cont exp
            Right spec -> run_directives_phrases opts [Right (PFrames spec [])] cont exp
        False -> catchIOError (Right <$> readFile file) handler >>= \case 
          Left err  -> putStrLn err >> cont exp
          Right str -> case parse_component syn_directives_phrases str of
            Left err -> putStrLn err >> cont exp
            Right eps -> run_directives_phrases opts eps cont exp
 where handler :: IOError -> IO (Either String a)
       handler exc | isDoesNotExistError exc = return (Left ("unknown file: " ++ fp))
                   | isPermissionError exc = return (Left ("cannot read: " ++ fp))
                   | isAlreadyInUseError exc = return (Left ("in use: " ++ fp))
                   | otherwise               = return (Left (show exc))



init_server :: Options -> PortNumber -> Spec -> Refiner -> Initialiser -> IO () 
init_server opts port_nr spec' ref' init' = do
  case compile_all spec' ref' init' [] of
    Left errs -> putStrLn "cannot compile specification" >> putStrLn (unlines errs)
    Right (spec', ref, init, _) -> do
      let spec = refine_specification spec' ref
      state <- make_initial_state spec init
      let explorer = init_explorer spec state
      start_server opts port_nr explorer

start_server :: Options -> PortNumber -> Cont ()
start_server opts port_nr explorer = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      Network.Socket.bind sock (SockAddrInet port_nr (tupleToHostAddress (0,0,0,0)))
      listen sock 2
      server opts sock  explorer

server :: Options -> Socket -> Cont ()
server opts = continue 
  where continue :: Socket -> Cont ()
        continue sock exp = do
          putStrLn "--- AWAITING STATEMENT ---"
          conn <- accept sock
          handle <- socketToHandle (fst conn) ReadWriteMode
          string <- hGetLine handle
          let (_, _, (sid, ctx)) = get_last_edge exp (EI.currRef exp)
          putStrLn string
          let compile_and phrase = report $ run_ exp (Execute [phrase])
              report res = case res of
                ResultTrans exp outs (old,oid) (new,sid) -> report_success sock outs oid old sid new exp
                Path path                        -> do
                  hPutStrLn handle (json_encode (GivePath path))
                  hClose handle
                  continue sock exp
--                Explorer.ExecError err    -> report_error (ExecError err) exp
                Nodes nodes -> do 
                  hPutStrLn handle (json_encode (GiveNodes nodes))
                  hClose handle
                  continue sock exp
                InvalidRevert             -> report_error InvalidState exp
--                CompilationError err      -> report_error (InvalidInput err) exp
              report_success sock outputs old_id c0 state_id ctx exp = do
                let viols = S.fromList (violations outputs)
                let outs = S.fromList (ex_triggers outputs)
                let errs = S.fromList (errors outputs)
                let qress = query_ress outputs
                let transitions = S.fromList (rest_transitions ctx)
                let new_duties = S.fromList (rest_duties ctx) S.\\ S.fromList (rest_duties c0)
                let new_enabled = S.fromList (rest_enabled ctx) S.\\ S.fromList (rest_enabled c0)
                let new_disabled = S.fromList (rest_disabled ctx) S.\\ S.fromList (rest_disabled c0)
                let all_duties = S.fromList $ rest_duties ctx
                hPutStrLn handle (json_encode (CommandSuccess state_id viols outs errs qress new_duties all_duties new_enabled new_disabled transitions))
                hClose handle
                continue sock exp
              report_error err exp = do 
                hPutStrLn handle (json_encode err)
                hClose handle
                continue sock exp
          let do_event event_type term = compile_and (event_type term)
          let withCommand cmd = case cmd of 
                CreateEvent term    -> do_event (Create []) term
                TerminateEvent term -> do_event (Terminate []) term 
                QueryCommand term   -> compile_and (PQuery term)
                Revert new_state    -> report $ run_ exp (Explorer.Revert new_state)
                Status mid          -> report $ run_ exp (Display (maybe (EI.currRef exp) id mid))
                History mid         -> report $ run_ exp (DisplayFull (maybe (EI.currRef exp) id mid))
                Heads               -> report $ run_ exp ExplorationHeads
                GetFacts            -> do hPutStrLn handle (json_encode (GiveFacts (state_holds (cfg_state ctx))))
                                          hClose handle
                                          continue sock exp
                Kill                -> hPutStrLn handle (json_encode ByeBye) >> hClose handle 
                ActionCommand d a r os force -> compile_and (PTrigger force [] term)
                  where term = App d (Left (a : r : os))
                CmdTrigger t b      -> compile_and (PTrigger b [] t)
                StringInstances d is -> report $ run_ exp (Execute [phrase])
                  where phrase = PDecl (FactTypeDecl False d (Strings is) (BoolLit True) Nothing)
                IntInstances d is -> report $ run_ exp (Execute [phrase])
                  where phrase = PDecl (FactTypeDecl False d (Ints is) (BoolLit True) Nothing)
                Phrase str  -> case parse_component syn_phrases str of
                  Left err  -> do hPutStrLn handle (json_encode (InvalidInput err))
                                  hClose handle >> continue sock exp
                  Right ps  -> report $ run_ exp (Execute ps)
          case eitherDecode (Data.ByteString.Lazy.Char8.pack string) of 
            Left err -> do when (find debug opts) (putStrLn err)
                           case (find accept_phrases opts) of
                            False -> do hPutStrLn handle (json_encode (InvalidCommand err))
                                        hClose handle
                                        continue sock exp
                            True  -> withCommand (Phrase string) 
            Right cmd-> withCommand cmd 
                 
json_encode r = Data.ByteString.Lazy.Char8.unpack (encode r)

data Command    = ActionCommand DomId Term Term [Term] Bool
                | CmdTrigger Term Bool
                | CreateEvent Term
                | TerminateEvent Term
                | QueryCommand Term
                | StringInstances DomId [String]
                | IntInstances DomId [Int]
                | Revert Int 
                | Status (Maybe Int)
                | Kill
                | GetFacts
                | History (Maybe Int)
                | Heads
                | Phrase String

instance FromJSON Command where
  parseJSON = withObject "Command" $ \v -> do 
                cmd <- v .: "command"
                case cmd::String of 
                  "create"      -> CreateEvent . value_to_term <$> v .: "value"
                  "terminate"   -> TerminateEvent . value_to_term <$> v .: "value"
                  "test-present"-> QueryCommand . value_to_term <$> v .: "value"
                  "test-absent" -> QueryCommand . Not . value_to_term <$> v .: "value"
                  "enabled"     -> QueryCommand . Enabled . value_to_term <$> v .: "value"
                  "revert"      -> Revert <$> v .: "value"
                  "action"      -> full_action <|> trigger_action
                    where full_action = 
                            actionCommand <$> 
                                    v .: "act-type" <*> v .: "actor" <*> v .: "recipient"
                               <*>  v .: "objects"  <*> maybe_force v
                          trigger_action = CmdTrigger . value_to_term <$> v .: "value" <*> maybe_force v 
                          actionCommand d a r os = ActionCommand d (to_term a) (to_term r) (map to_term os)
                  "status"      -> Status <$> v .: "state"
                                <|> return (Status Nothing)
                  "history"     ->  History <$> v .: "state"
                                <|> return (History Nothing)
                  "trace-heads" -> return Heads
                  "kill"        -> return Kill
                  "phrase"      -> Phrase <$> v .: "text"
                  "event"       -> CmdTrigger . value_to_term <$> v .: "value" <*> maybe_force v 
                  "instances-of"-> StringInstances <$> v .: "fact-type" <*> v .: "instances"
                               <|> IntInstances <$> v .: "fact-type" <*> v .: "instances"
                  "facts"       -> return GetFacts
                  _             -> mzero

maybe_force v = v .: "force" <|> return False

data Value      = Atom DomId (Either String Int)
                | Composite DomId [Value]

data StringOrValue = ST String | VT Value

instance FromJSON StringOrValue where
  parseJSON v = case v of 
    JSON.String str -> return (ST (unpack str))
    JSON.Object obj -> VT <$> parseJSON v 
    _               -> fail ("looking for a string or a value, not a " ++ show v)

to_term :: StringOrValue -> Term
to_term (ST s) = StringLit s
to_term (VT v) = value_to_term v

tag_of :: Value -> DomId
tag_of (Atom d _) = d
tag_of (Composite d _) = d

value_to_term :: Value -> Term
value_to_term v = case v of
  Atom d (Left s)  -> App d (Left [StringLit s]) 
  Atom d (Right i) -> App d (Left [IntLit i])
  Composite d vs   -> App d (Right $ map value_to_modifier vs)

value_to_modifier :: Value -> Modifier
value_to_modifier v = Rename (no_decoration (tag_of v)) (value_to_term v)

instance FromJSON Value where
  parseJSON = withObject "Value" $ \v -> 
                    (\c i -> Atom c (Right i)) <$> v .: "fact-type" <*> v .: "value"
                <|> (\c s -> Atom c (Left s))  <$> v .: "fact-type" <*> v .: "value"
                <|> Composite <$> v .: "fact-type" <*> v .: "value"
                <|> Composite <$> v .: "fact-type" <*> v .: "arguments"

data Response   = InvalidCommand String
                | InvalidInput String -- parse error
                | CommandSuccess Int (S.Set Violation) 
                                     (S.Set OutputEvent)
                                     (S.Set Error) -- errors: compilation + transition
                                     [QueryRes]         -- query results 
                                     (S.Set Tagged) -- new duties
                                     (S.Set Tagged) -- all duties in the current state 
                                     (S.Set Tagged) -- newly enabled transitions 
                                     (S.Set Tagged) -- newly disabled transitions
                                     (S.Set (Tagged, Bool)) -- all transitions 
                | InvalidState
                | GiveFacts [Tagged]
                | GivePath Path
                | GiveNodes [Node]
                | ByeBye

instance ToJSON Response where
  toJSON (InvalidCommand err) = object [ "response" .= JSON.String "invalid command", "message" .= toJSON err ]
  toJSON (CommandSuccess i vs outs errs qress new_duties all_duties new_enabled new_disabled all_transitions) = 
    object [ "response"   .= JSON.String "success"
           , "violations" .= toJSON vs
           , "output-events" .= toJSON outs
           , "errors"     .= toJSON errs
           , "query-results" .= toJSON qress
           , "new-state"  .= toJSON i
           , "new-duties" .= toJSON (map TaggedJSON $ S.toList new_duties)
           , "new-enabled-transitions" .= toJSON (map TaggedJSON $ S.toList new_enabled)
           , "new-disabled-transitions" .= toJSON (map TaggedJSON $ S.toList new_disabled)
           , "all-duties" .= toJSON (map TaggedJSON $ S.toList all_duties)
           , "all-disabled-transitions" .= toJSON (map TaggedJSON dis_transitions)
           , "all-enabled-transitions" .= toJSON (map TaggedJSON en_transitions)
           ]
   where en_transitions = map fst $ filter snd $ S.toList all_transitions 
         dis_transitions = map fst $ filter (not . snd) $ S.toList all_transitions 
  toJSON InvalidState       = object [ "response" .= JSON.String "invalid state" ]
  toJSON (InvalidInput err) = object [ "response" .= JSON.String "invalid input" 
                                     , "error"    .= toJSON err ]
  toJSON ByeBye             = object [ "response"  .= JSON.String "bye world.." ]
  toJSON (GiveFacts tes)    = object [ "values" .= toJSON (map TaggedJSON tes) ]
  toJSON (GiveNodes nodes)  = object [ "nodes"  .= toJSON (map toJSONNode nodes) ]
    where toJSONNode (sid, cfg) = 
            object [ "state_id"       .= toJSON sid
                   , "state_contents" .= toJSON (map TaggedJSON (state_holds (cfg_state cfg))) ]
  toJSON (GivePath edges)   = object [ "edges"  .= toJSON (map toJSONEdge edges') ]
    where edges' = sortBy (on compare (\((sid,_),_,_) -> sid)) edges
          toJSONEdge ((sid_from,ctx_from), (phr, output), (sid_to, ctx_to)) = 
            object [ "source_id"        .= toJSON sid_from
                   , "source_contents"  .= toJSON (map TaggedJSON facts_from)
                   , "phrase"           .= toJSON (ppPhrase phr)
                   , "target_id"        .= toJSON sid_to
                   , "target_contents"  .= toJSON (map TaggedJSON facts_to)
                   , "created_facts"    .= toJSON (map TaggedJSON created)
                   , "terminated_facts" .= toJSON (map TaggedJSON terminated)
                   , "violations"       .= toJSON rep_viols
                   , "output"           .= toJSON output
                   ]
           where facts_from = state_holds (cfg_state ctx_from)
                 facts_to   = state_holds (cfg_state ctx_to)
                 created    = facts_to \\ facts_from
                 terminated = facts_from \\ facts_to  
                 rep_viols  = violations output 

instance ToJSON Violation where
  toJSON (TriggerViolation te is_action) =
    object [ "violation" .= JSON.String "trigger"
           , "value"     .= toJSON (TaggedJSON te)
           , "is-action" .= JSON.Bool is_action ]
  toJSON (DutyViolation te) = 
    object [ "violation" .= toJSON ("duty"::String)
           , "value"     .= toJSON (TaggedJSON te) ]
  toJSON (InvariantViolation d) = 
    object [ "violation" .= toJSON ("invariant"::String)
           , "invariant" .= toJSON d ]

instance ToJSON OutputEvent where
  toJSON (ExecutedTransition te action) = 
    object [ "type"       .= JSON.String "executed-transition"
           , "value"      .= toJSON (TaggedJSON te)
           , "is-action"  .= JSON.Bool action]

instance ToJSON QueryRes where
  toJSON qres = case qres of
    QuerySuccess -> "success"
    QueryFailure -> "failure"

instance ToJSON Error where
  toJSON err = case err of
    NonDeterministicTransition -> object [ "error-type" .= JSON.String "non-deterministic transition"]
    DisabledTransition te -> object ["error-type" .= JSON.String "disabled transition"
                                    ,"value" .= TaggedJSON te ]
    CompilationError err  -> object ["error-type" .= JSON.String "compilation error"
                                    ,"error" .= toJSON err]

instance ToJSON Output where
  toJSON (OutputEvent e)  = toJSON e
  toJSON (Violation v)    = toJSON v
  toJSON (QueryRes r)     = toJSON r
  toJSON (ErrorVal e)     = toJSON e
