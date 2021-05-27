# eFLINT API

The goal of this repository is to run custom code on top of eFLINT and provide a way to interact with the code.

## Steps to run the e-FLINT code

Update the repository and the submodules:

```bash
git pull
git submodule update --init
```

Then, build the e-FLINT server and a Jupyter Lab server using the following command (also after doing a `git pull`):

```bash
docker-compose build
```

And then, run the e-FLINT server and Jupyter Lab server:

```bash
docker-compose up
```

After loading, it should be possible to access [localhost:9999](http://localhost:9999/) in the browser on which Jupyter Lab runs.

## Development

Follow the steps in the `eflint/README.md` file for a local installation of eFLINT. Make sure to add the `eflint-repl` executable to your `/usr/local/bin` such that it can be executed.

## e-FLINT documentation

e-FLINT documentation can be found on the [e-FLINT GitLab](https://gitlab.com/eflint/haskell-implementation).