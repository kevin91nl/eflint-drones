# eFLINT API

The goal of this repository is to run custom code on top of eFLINT and provide a way to interact with the code.

## Steps to run the e-FLINT code

Update the repository and the submodules:

```bash
git pull
git submodule update --init
```

Then, build the eFLINT API using the following command (this might take a while):

```bash
docker build . --tag eflint-api
```

Afterwards, you can run the eFLINT API:

```bash
docker run eflint-api -p5000:5000
```

## Development

Follow the steps in the `eflint/README.md` file for a local installation of eFLINT. Make sure to add the `eflint-repl` executable to your `/usr/local/bin` such that it can be executed.