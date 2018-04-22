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
        echo -n "Analyzing $cfile ... " >>eba-linux.log
        /usr/bin/time -a -o eba-linux.log --format="%E" timeout 15m \
            eba -L --warn-output --externs-do-nothing ${tfile} \
            >>eba.log 2>&1 || true
        rm -f "${tfile}" "${ifile}"
        rm -f -d "$(dirname ${tfile})"
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

echo "NOTE: This may take several hours." >&2
echo -ne "OK, let's analyze...\r" >&2
file_count=0
find "$linuxdir" -type f -name '*.c' -print0 | \
    while IFS= read -r -d '' file; do
        analyze "$file"
        file_count=$((file_count + 1))
        echo -ne "Analyzed $file_count files...\r" >&2
    done
echo -ne "\n"
echo "Done!" >&2
