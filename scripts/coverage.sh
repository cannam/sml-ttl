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

# Mangle the output of mlprof into a series of lines of the form
# filename,lineno,yes (or no)
# indicating whether the given line of the given file has been
# executed.

# Mlprof sometimes outputs more than one result for a given source
# line; we want to remove these duplicates because we use the line
# count to calculate our %ages, and also if it outputs both "yes" and
# "no", we want to keep only the "yes". The "sort -r | perl" business
# does that, by sorting on filename and line and then with "yes"
# before "no", and then keeping only the first in any sequence of
# lines with a common filename and line.

mlprof -raw true -show-line true "$PROGRAM" mlmon.out |
    grep '/.*sml: [0-9]' |
    sed 's,^.* \([a-z][a-z]*\)/,\1/,' |
    sed 's/: / /' |
    awk '{ print $1","$2","$4 }' |
    sed 's/(0)/no/g' |
    sed 's/([0-9,]*)/yes/g' |
    sort -r |
    perl -e 'while (<>) { ($f, $n, $b) = split /,/; next if ($f eq $pf and $n eq $pn); print; $pf = $f; $pn = $n }' > "$tmpfile"

summarise_for() {
    what="$1"
    yes=$(fgrep "$what" "$tmpfile" | grep ",yes$" | wc -l)
    no=$(fgrep "$what" "$tmpfile" | grep ",no$" | wc -l)
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
    expand_arg "$mlb" | grep -v '^/' | grep -v '\.sig$' | sort |
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
	    if grep -q "$srcfile,$n,no" "$tmpfile" ;
	    then echo " ### $x";
	    else echo "     $x";
	    fi;
	done | \
	grep -C2 '^ ###'
fi

