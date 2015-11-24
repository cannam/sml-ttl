#!/bin/bash

set -eu

mlton -profile count -profile-branch true tests.mlb
./tests >/dev/null

tmpfile=/tmp/"$$"_cov
trap "rm -f $tmpfile" 0

mlprof -raw true -show-line true tests mlmon.out |
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
	echo "  --%  $what"
    else 
	percent=$(((100 * $yes) / $total))
	if [ "$percent" = 100 ]; then
	    echo " 100%  $what"
	else 
	    echo "  $percent%  $what"
	fi
    fi
}

summarise_for "sml"
find src -name \*.sml -print | sort | while read x; do summarise_for "$x" ; done
