# EBA: An effective bug finder for C

EBA is a prototype bug finder for C based on side-effect analysis and model-checking.

For now, you can use it to find double-lock bugs in the Linux kernel:

    git clone --depth=1 https://github.com/torvalds/linux.git
    cd linux
    make allyesconfig
    scripts/eba-linux.sh drivers/

The script will find all C source files under `drivers/`, call CPP on them, and call EBA to find potential double-locks. Be patient, this may take several hours.

The preprocessed files and the findings of EBA are put under the `_eba` sub-directory. If EBA finds some potential bug in `_eba/path/to/file.c`, it will write the bug traces in `_eba/path/to/file.warn`:

    find _eba/ -iname '*.warn'

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
