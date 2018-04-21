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

See the [Installation instructions](INSTALL.md).

## Running the tests

If you want to run the tests you will need to install _cram_, for instance using _pip_:

    sudo apt-get install python-pip
    sudo pip install cram

You should place _eba_ somewhere in your _$PATH_:

    cram test/*.t
