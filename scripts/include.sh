#!/bin/bash

if [ -z "${SML_LIB:-}" ]; then
    lib=/usr/lib/mlton/sml
    if [ ! -d "$lib" ]; then
	lib=/usr/local/lib/mlton/sml
    fi
else
    lib="$SML_LIB"
fi

simplify() {
    sed -e 's,^./,,' -e 's,[^/][^/]*/../,,g' -e 's,//,/,g'
}

cat_mlb() {
    local mlb="$1"
    if [ ! -f "$mlb" ]; then
	echo "*** Error: File not found: $mlb" 1>&2
	exit 1
    fi
    local dir=$(dirname "$mlb");
    cat "$mlb" | while read line; do
	local trimmed=$(
	    echo "$line" | 
		sed 's/(\*.*\*)//' |              # remove ML-style comments
		sed 's#$(SML_LIB)#'"${lib}"'#g' | # expand library path
		sed 's/^ *//' |                   # remove leading whitespace
		sed 's/ *$//')                    # remove trailing whitespace
	local path="$trimmed"
	case "$path" in
	    /*) ;;
	    *) path="$dir/$trimmed" ;;
	esac
	case "$path" in
	    "") ;;		                  # ignore empty lines
	    *basis.mlb) ;;			  # remove incompatible Basis lib
	    *main.sml) ;;			  # remove redundant call to main
	    *.mlb) cat_mlb "$path" ;;
	    *.sml) echo "$path" | simplify ;;
	    *.sig) echo "$path" | simplify ;;
	esac
    done
}

expand_arg() {
    local arg="$1"
    case "$arg" in
	*.sml) echo "$arg" ;;
	*.mlb) cat_mlb "$arg" ;;
	*) echo "*** Error: .sml or .mlb file must be provided" 1>&2
	   exit 1 ;;
    esac
}

get_outfile() {
    local arg="$1"
    case "$arg" in
	*.sml) basename "$arg" .sml ;;
	*.mlb) basename "$arg" .mlb ;;
	*) echo "*** Error: .sml or .mlb file must be provided" 1>&2
	   exit 1 ;;
    esac
}

