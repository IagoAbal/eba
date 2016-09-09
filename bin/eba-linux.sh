#!/bin/bash

# Prerequisites:
# - Linux kernel source already configured (eg. make allyesconfig).
# - An script called eba-check in your $PATH that does the right thing.
#
# Given a number of parallel jobs, and a Linux kernel directory, this script
# iterates through all the .c files in that directory, preprocess them (copying
# the result to _eba/path/to/file.c), and calls eba-check on them.
#
# Contrary to using make, this script analyzes any file that can be preprocessed,
# even if it cannot or should not be build.

JOBS="$1"
DIR="$2"

analyze() {
   CFILE="$1"
   IFILE="${CFILE%.*}.i"
   TFILE="_eba/$CFILE"
   # Preprocess the .c file if no .i exists
   if [ ! -f "$IFILE" ]; then
       make "$IFILE" >/dev/null 2>&1
   fi
   # If preprocessing worked, analyze it.
   if [ -f "$IFILE" ]; then
       mkdir -p "$(dirname $TFILE)"
       cp --link "$IFILE" "$TFILE"
       eba-check "$TFILE"
   fi
}

export -f analyze

function usage {
    echo
    echo "Usage:  $0 JOBS DIR"
    echo "EBA-checks the kernel DIR in parallel."
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

files=()
while IFS= read -r -d $'\0' file; do
    files+=("$file")
done < <(find "$DIR" -type f -name '*.c' -print0)

parallel -j "$JOBS" analyze ::: "${files[@]}"
