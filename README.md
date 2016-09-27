# The EBA bug finder

EBA is a proof-of-concept bug finder.

For now, you can use it to find double-lock bugs in the Linux kernel. In order to check _path/to/file.c_ do:

    make path/to/file.i    # EBA runs on single preprocessed files !
    eba -L path/to/file.i

There are some options you can use:

    eba --help

In the _bin_ folder there are some scripts to run EBA along with the build process.

## Bugs found

So far I've only used EBA to analyze the Linux 4.7 release.

I can truly claim credit for these bugs:

- [HSI: cmt_speech: Fix double spin_lock](https://github.com/torvalds/linux/commit/3c13ab1d96e1924ef73b1a20c1ccccc993b6fb58)
- [usb: gadget: pch_udc: reorder spin_un/lock to avoid deadlock](https://github.com/torvalds/linux/commit/1d23d16a88e6c8143b07339435ba061b131ebb8c)

EBA has found another two confirmed double-lock bugs, but I was slow at reporting them and they got fixed in the meantime. Too bad.

EBA reported another two double-lock bugs that, while supposedly could not occur in practice, lead to style and dead-code elimination bug-fixes.

There are three more waiting any kind of confirmation by Linux maintainers, and a few more that I haven't report yet.

## Installation

You will need OCaml 4.01 (see [https://github.com/cil-project/cil/issues/18](https://github.com/cil-project/cil/issues/18)), preferrably just install [OPAM](http://opam.ocaml.org).

Download the source code.

Install _omake_ if necessary.

    sudo apt-get install omake

Install dependencies

    opam install batteries cil smart-print cmdliner dolog ocamlgraph

Invoke omake (you build in parallel using -j):

    omake

## Running the tests

If you want to run the tests you will need to install _cram_, for instance using _pip_:

    sudo apt-get install python-pip
    sudo pip install cram

You should rename _eba.opt_ to _eba_ and move it somewhere in your _$PATH_, otherwise create a symlink.

Finally,

    omake test
