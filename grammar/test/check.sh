#!/bin/bash

# Looks for code separated by \n\n
# , generates a PNG of the AST
# , checks it against a previously generated PNG.

[[ $# -eq 0 ]] && echo "Usage:  $0 ‹a_file.kju›"

# Enable job control
set -m
#   http://stackoverflow.com/a/12777848/1418165

file="$1"; k=0
#[[ $# -eq 2 ]] && file="$2" && k="$1"

code=''; i=1; T=test
cat "$1" | while read line
do
    if [[ '' = "$line" ]]; then
        #[[ $k -ne 0 ]] && [[ $i -ne $k ]] && continue
        echo -ne "Code $i:\n$code\n"
        echo "$code" | java -Xmx8g org.antlr.v4.runtime.misc.TestRig Kju root -encoding utf8 -tree > $T/_$i.tree
        diff -u $T/$i.tree $T/_$i.tree
	if [[ $? -ne 0 ]]; then
            echo "$code"
            open $T/$i.png
            echo "$code" | java -Xmx8g org.antlr.v4.runtime.misc.TestRig Kju root -encoding utf8 -gui
        fi
        code=''; ((i++))
        echo
    else
        if [[ '' = "$code" ]]; then
            code="$line"
        else
            code="$code
$line"
        fi
    fi
done
