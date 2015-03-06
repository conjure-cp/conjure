#!/bin/bash
set -o nounset
# Usage save_choices <filename.json>  <conjure command>
# Save the choices made in json in $1
# Need to set the --logLevel to anything but lognone e.g. --log-level=logfollow

file="$1"
shift

"$@" | tee >( grep '^LF:' | grep 'END:$' \
		| sed -e 's/END://' | sed -e '1s/LF:/[/;t' -e '1,/LF:/s//[/' -e's/LF:/,/1'  \
		> "$file" && ( [ -s "$file" ] && echo ']' >> "$file") ) \
	| grep -v '^LF:'