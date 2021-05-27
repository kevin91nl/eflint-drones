FROM haskell 

# Build eFLINT
RUN mkdir /tmp/eflint-project
WORKDIR /tmp/eflint-project
RUN cabal update 
COPY eflint .
RUN rm src/Server.hs
COPY Server.hs src/Server.hs
RUN cabal install
RUN cp /root/.cabal/bin/eflint-server /usr/bin/eflint-server
RUN cp /root/.cabal/bin/eflint-repl /usr/bin/eflint-repl
RUN rm -rf /tmp/eflint-project

# Add the examples
COPY ./data /data

EXPOSE 5000
ENTRYPOINT ["eflint-server", "/data/in-flight.eflint", "5000", "--debug"]