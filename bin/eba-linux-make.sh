#!/bin/bash

# Adapted from Smatch's test_kernel.sh script.

JOBS="$1"

function usage {
    echo
    echo "Usage:  $0 JOBS"
    echo "Compiles-and-checks the kernel with -jJOBS"
    echo
    echo "You must have an eba-check in your \$PATH that does the right thing."
    echo
    exit 1
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then
	usage;
fi

if ! which eba-check | grep eba-check > /dev/null ; then
    echo "Cannot find your eba-check script!"
    exit 1
fi

make clean
find -name \*.c.warn -exec rm \{\} \;
make -j"$JOBS" -k CHECK="eba-gcc" C=1 bzImage modules 2>&1 \
        | tee cc.warns
find -name \*.c.warn -exec cat \{\} \; -exec rm \{\} \; > eba.warns

echo "Done.  The warnings are saved to cc.warns and eba.warns"
