
You will need to install [OPAM](http://opam.ocaml.org), for instance, via APT on Ubuntu:

    apt install -y opam

If you have set up OPAM in the past, you can probably skip the next step. EBA should work with any 4.x version of OCaml---let me know if it doesn't.

If you are installing OPAM for the first time:

    opam init --auto-setup
    opam switch 4.04.2
    eval `opam config env`

Setup customized CIL for EBA:

    git clone https://github.com/IagoAbal/eba-cil.git
    git checkout 653136911cb4cde474d7a322e8d05c4e7674acbd # optional
    opam pin add -n cil eba-cil/

Install EBA itself:

    git clone https://github.com/IagoAbal/eba.git
    git checkout v1.0 # optional
    opam pin add -y -n eba eba/
    opam install eba

You should find the binary in your `$PATH`, if not:

    . ~/.profile

The binary should be in `~/.opam/<OCaml version>/bin/eba`.

Alternatively:

    git clone git@github.com:IagoAbal/eba.git
    opam pin add -y -n eba eba/
    opam install eba --deps-only
    cd eba
    make

And you will find the binary in `bin/eba`.

Enjoy!
