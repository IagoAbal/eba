# EBA: An effective bug finder for C

EBA is a prototype bug finder for C based on side-effect analysis and model-checking.

For now, you can use it to find double-lock bugs in the Linux kernel. In order to check _path/to/file.c_ do:

    make path/to/file.i    # EBA runs on single preprocessed files !
    eba -L path/to/file.i

There are some options you can use:

    eba --help

In the _bin_ folder there are some scripts to run EBA along with the build process.

### Hows does it work?

It combines side-effect analysis and model-checking, check the website for more info: [http://www.iagoabal.eu/eba/](http://www.iagoabal.eu/eba/)

### Does it really find bugs?

Yes, it really does, check the website for more info: [http://www.iagoabal.eu/eba/](http://www.iagoabal.eu/eba/)

## Installation

You will need OCaml 4.01 (see [https://github.com/cil-project/cil/issues/18](https://github.com/cil-project/cil/issues/18)), preferrably just install [OPAM](http://opam.ocaml.org).

Setup customized CIL for EBA:

    git clone git@github.com:IagoAbal/eba-cil.git
    opam pin add -n cil eba-cil/

Install dependencies

    opam install batteries cil smart-print cmdliner dolog ocamlgraph

Download the source code.

Install _omake_ if necessary.

    sudo apt-get install omake

Invoke omake (you build in parallel using -j):

    omake

## Running the tests

If you want to run the tests you will need to install _cram_, for instance using _pip_:

    sudo apt-get install python-pip
    sudo pip install cram

You should rename _eba.opt_ to _eba_ and move it somewhere in your _$PATH_, otherwise create a symlink.

Finally,

    omake test
