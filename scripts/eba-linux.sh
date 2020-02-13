#!/bin/bash

#
# Copyright 2016--present, Iago Abal
#

# This is a utility script for running the EBA bug finder on Linux.
#
# It relies on Make to figure out what files need to be (re)analyzed. Files are
# preprocessed before being analyzed and EBA's warnings are stored in .warn
# in the _eba folder. When a file is re-analyzed, the script compares previous
# and current warnings and reports any change.
#
# NOTE: This heavily relies on a few features of the Linux build scripts,
#       namely the ability to `make CHECK="command"` and `make path/to/file.i`.

readonly this_script="$0"

# Global variables
##################################################

export eba="eba"
export eba_folder="_eba"
export eba_log="$eba_folder/eba-linux.log"
export make_log="$eba_folder/make.log"
export eba_list_of_files_to_analyze="$eba_folder/c-files-to-analyze"
export new_bugs="$eba_folder/new_bugs"

# Command line
##################################################

readonly usage="
Usage: $this_script [-j N] [-t T] [-r] [-i] [DIR [DIR ...]]

Analyze Linux C sources for double-locks using EBA.

NOTE: Options must be passed in the specified order!

Options:
-j N    Use N parallel jobs (by default =1).
-t T    Timeout for EBA.
-r      Reanalyze the same set of files as the last run.
-i      Report every bug alarm ignoring previous ones.
-h      Print this help message and exit.

Prerequisites:
  - Linux kernel source is properly configured (e.g., \`make allyesconfig\`).
  - The '$eba' binary is located in your \$PATH.

NOTE: There is an internal 'list' mode that is used by the script invoking
itself through \`make CHECK=\"...\"\` in order to collect the list of files to
(re)analyze. Shouldn't be relevant to you, but may be good to be aware of.
"

# INTERNAL USE ONLY (but be aware)
# "list" mode, used to collet the list of files to analyze
if [ "$1" = "list" ]; then
    readonly logfile="$2"
    readonly cfile="${@: -1}"

    echo "$cfile" >>"$logfile"

    exit 0
fi

jobs=1
if [ "$1" = "-j" ]; then
    shift
    jobs="$1"
    shift
fi

eba_timeout="15m"
if [ "$1" = "-t" ]; then
    shift
    eba_timeout="$1"
    shift
fi
export eba_timeout

reanalyze=false
if [ "$1" = "-r" ]; then
    reanalyze=true
    shift
fi

ignore_old=false
if [ "$1" = "-i" ]; then
    ignore_old=true
    shift
fi
export ignore_old

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "$usage" >&2
    exit 1
fi

# Sanity checks
##################################################

if ! command -v "$eba" > /dev/null; then
    echo "Cannot find your '$eba' binary, please add it to your \$PATH." >&2
    exit 1
fi

if [ ! -f .config ]; then
    echo "Cannot find file ./.config, have you configured the Kernel?" >&2
    exit 1
fi

# Analyze a C file
##################################################

analyze() {
    local cfile="${@: -1}"
    local ifile="${cfile%.c}.i"
    local tfile="$eba_folder/${cfile}"
    local wfile="$tfile.warn"
    local old_wfile="$(dirname ${wfile})/old.$(basename ${wfile})"

    # Preprocess the file to analyze
    make "${ifile}" >>"$make_log" 2>&1
    mkdir -p "$(dirname ${tfile})"
    mv "${ifile}" "${tfile}" 2>/dev/null || true

    # Note that preprocessing may fail...
    if [ -f "$tfile" ]; then
        # but if it succeeds then we analyze!

        [ -f "$wfile" ] && mv "$wfile" "$old_wfile"

        # EBA doesn't support _Static_assert yet, and it doesn't really matter,
        # so let's delete it...
        sed -i '/^_Static_assert/ d' "$tfile"
        sed -i 's/asm __inline volatile/asm volatile/g' "$tfile"

        echo "Analyzing $cfile ... " >>"$eba_log"
        /usr/bin/time -a -o "$eba_log" --format="%E" timeout "$eba_timeout" \
            "$eba" -L --warn-output --externs-do-nothing "$tfile" \
            >>"$eba_log" 2>&1 || true

        if [ -f "$wfile" ]; then
            local dup=false;
            if [ -f "$old_wfile" ] && ! diff -Bw "$wfile" "$old_file" &>/dev/null; then
                dup=true
            fi
            if [ "$dup" = false ] || [ "$ignore_old" = true ]; then
                echo "Yeheey, we may have found a new bug in $cfile!"
                cat "$wfile" >>"$new_bugs"
            fi
        fi

        rm -f "${tfile}"
    fi

    if [ -d "$(dirname ${tfile})" ] && [ ! -f "$wfile" ]; then
        rm -f -d "$(dirname ${tfile})" 2>/dev/null
    fi
}

export -f analyze

# Main
##################################################

show_progress=false
if [ -t 1 ]; then
    # stdout is a terminal
    show_progress=true
fi

echo "NOTE: This may take several hours."
mkdir -p "$eba_folder/"
[ -f "$make_log" ] && mv "$make_log" "$make_log.bak"
[ -f "$eba_log" ] && mv "$eba_log" "$eba_log.bak"
[ -f "$new_bugs" ] && mv "$new_bugs" "$new_bugs.bak"
touch "$make_log" "$eba_log" "$new_bugs"
echo OK?
if [ "$reanalyze" = false ]; then
    echo OK
    [ -f "$eba_list_of_files_to_analyze" ] && mv "$eba_list_of_files_to_analyze" "$eba_list_of_files_to_analyze.bak"
    touch "$eba_list_of_files_to_analyze"
    echo "Collecting files to analyze..."
    make -j"$jobs" C=1 CHECK="$0 list $eba_list_of_files_to_analyze" "$@" &>>"$make_log"
fi
readonly new_files=$(wc -l <"$eba_list_of_files_to_analyze")
echo "NOTE: Bug reports will be gathered in $new_bugs."
echo "There are $new_files files to analyze, let's go!"
readarray -t files_to_analyze < "$eba_list_of_files_to_analyze"
file_count=0
[ "$show_progress" = true ] && echo -ne "Analyzed $file_count files..."
for file in "${files_to_analyze[@]}"
do
    sem -j "$jobs" -- analyze "$file"
    file_count=$((file_count + 1))
    [ "$show_progress" = true ] && echo -ne "\rAnalyzed $file_count files..."
done
echo " done."
echo "Bon appetit!"
