#!/bin/bash

#
# Copyright 2019--present, Iago Abal
#

set -eo pipefail

readonly usage="
Usage: $0 [-n N]

Utility script for running the EBA bug finder on Linux Next incrementally.
It runs EBA on the set of drivers/ files that have changed in between the
the _N_ latest next-* tags.

Options:
-n N    Check N tags back (by default =1).
-h      Print this help message and exit.

Prerequisites:
  - Linux kernel source is properly configured (e.g., \`make allyesconfig\`).
  - The '$eba' binary is located in your \$PATH.
"

delta=1
if [ "$1" = "-n" ]; then
    shift
    delta="$1"
    shift
fi

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "$usage" >&2
    exit 1
fi

let nof_tags=delta+1

echo "$0: git fetching..."
git fetch origin --tags

mapfile -t next_tags < <( git tag -l next-* | sort -r | head -n $nof_tags )

if [ "${#next_tags[@]}" -lt 1 ]; then
    echo '$0: error: No next-* tags found!' >&2
    exit 1
fi

current="${next_tags[0]}"

if [ ${#next_tags[@]} -lt $nof_tags ]; then
    echo "$0: warning: Could only find tag $current, will analyze it from scratch."
    ./eba-linux.sh -j 2 drivers
else
    previous="${next_tags[$delta]}"
    echo "$0: I will analyze changes to $current wrt $previous."
    cp _eba/c-files-to-analyze _eba/c-files-to-analyze.bak
    git diff --name-only "$previous".."$current" | grep -E "^drivers.*\.c" >_eba/c-files-to-analyze
    ./eba-linux.sh -j 2 -r
fi