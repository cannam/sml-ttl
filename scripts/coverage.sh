#!/bin/bash

set -e

mlb="$1"
srcfile="$2"

if [ -z "$mlb" ]; then
    echo "Usage: $0 program.mlb" 1>&2
    echo "         prints coverage summary for running program.mlb" 1>&2
    echo "       $0 program.mlb file.sml" 1>&2
    echo "         prints detailed coverage for file.sml in program.mlb" 1>&2
    exit 1
fi

set -u

mydir=$(dirname "$0")
. "$mydir/include.sh"

PROGRAM=$(get_outfile "$mlb")

mlton -profile count -profile-branch true -profile-val true "$PROGRAM.mlb"
./"$PROGRAM" >/dev/null

tmpfile=/tmp/"$$"_cov
trap "rm -f $tmpfile" 0

mlprof -raw true -show-line true "$PROGRAM" mlmon.out |
    grep 'src/.*sml: [0-9]' |
    sed 's,^.* src/,src/,' |
    sed 's/: / /' |
    awk '{ print $1, $2, $4 }' |
    sed 's/(0)/no/g' |
    sed 's/([0-9,]*)/yes/g' > "$tmpfile"

summarise_for() {
    what="$1"
    yes=$(fgrep "$what" "$tmpfile" | grep "yes$" | wc -l)
    no=$(fgrep "$what" "$tmpfile" | grep "no$" | wc -l)
    total=$(($yes + $no))
    if [ "$total" = "0" ]; then
	echo "  --%  $what (0/0)"
    else 
	percent=$(((100 * $yes) / $total))
	if [ "$percent" = 100 ]; then
	    echo " 100%  $what ($yes/$total)"
	elif [ "$percent" -lt 10 ]; then
	    echo "   $percent%  $what ($yes/$total)"
	else 
	    echo "  $percent%  $what ($yes/$total)"
	fi
    fi
}

if [ "$srcfile" = "" ]; then

    summarise_for "sml"
    find src -name \*.sml -print | sort |
	while read x; do
	    summarise_for "$x" ;
	done

else 

    # A monumentally inefficient way to show the lines lacking
    # coverage from a given source file
    cat -n "$srcfile" |
	sed 's/^ *//' |
	while read x; do
	    n=${x%%[^0-9]*}
	    if grep -q "$srcfile $n no" "$tmpfile" ;
	    then echo " ### $x";
	    else echo "     $x";
	    fi;
	done | \
	grep -C2 '^ ###'
fi

