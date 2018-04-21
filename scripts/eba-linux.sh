#!/bin/bash

readonly linuxdir="$1"

analyze() {
    local cfile="${@: -1}"
    local ifile="${cfile%.c}.i"
    local tfile="_eba/${cfile}"

    if [ ! -f "$tfile" ]; then
        make "${ifile}" >>make.log 2>&1
        mkdir -p "$(dirname ${tfile})"
        cp --link "${ifile}" "${tfile}" 2>/dev/null || true
    fi

    if [ -f "$tfile" ]; then
        echo -n "Analyzing $cfile ... " >&2
        /usr/bin/time -o /dev/fd/3 --format="%E" timeout 15m \
            eba -L --warn-output --externs-do-nothing ${tfile} \
            3>&2 >>eba.log 2>&1 || true
    fi
}

export -f analyze

if [ "$1" = "-h" ] || [ "$1" = "--help" ] || 
[ "$#" -ne 1 ]; then
    echo "Usage: $0 DIR" >&2
    echo "" >&2
    echo "Analyze Linux C sources for double-locks using EBA." >&2
    echo "" >&2    
    echo "Prerequisites:" >&2
    echo "  - Linux kernel source already configured (e.g., make allyesconfig)." >&2
    echo "  - The 'eba' binary in your \$PATH." >&2
    exit 1
fi

if ! command -v eba > /dev/null; then
    echo "Cannot find your 'eba' binary!" >&2
    exit 1
fi

echo "OK, let's analyze... (this may take several hours)" >&2
find "$linuxdir" -type f -name '*.c' -print0 | \
    while IFS= read -r -d '' file; do
        analyze "$file"
    done
