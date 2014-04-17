#!/bin/bash

# Looks for code separated by \n\n
# , generates a PNG of the AST
# , checks it against a previously generated PNG.

[[ $# -eq 0 ]] && echo "Usage: [FROM=37] [T=test] $0 ‹a_file.kju›"

# Enable job control
set -m
#   http://stackoverflow.com/a/12777848/1418165

file="$1"; k=0
#[[ $# -eq 2 ]] && file="$2" && k="$1"

T=${T:-'test'}
FROM=${FROM:-0}

printf "\e[1;3m%s\e[0m\n" "Checking '$file'. (stop by removing the generated parser, ^C won't do)."

code=''; i=1
while read line
do
    if [[ '' = "$line" ]]; then
        [[ $FROM -ne 0 ]] && [[ $i -lt $FROM ]] && code='' && ((i++)) && continue
        #[[ $k -ne 0 ]] && [[ $i -ne $k ]] && continue
        echo "Snippet $i:"
        echo "	$code" | sed 's/\\n/\n\t/g'
        echo -e "$code" | java -Xmx8g org.antlr.v4.runtime.misc.TestRig Kju root -encoding utf8 -tree > $T/_$i.tree
        if [[ ! -f $T/$i.tree ]]; then
            printf "\e[1;3m%s\e[0m\n" "Add this new test under '$T/$i.{tree,png}'"
            cat $T/_$i.tree
            echo -e "$code" | java -Xmx8g org.antlr.v4.runtime.misc.TestRig Kju root -encoding utf8 -gui
        else
            diff -u $T/$i.tree $T/_$i.tree
	    if [[ $? -ne 0 ]]; then
                open $T/$i.png
                echo -e "$code" | java -Xmx8g org.antlr.v4.runtime.misc.TestRig Kju root -encoding utf8 -gui
            fi
        fi
        rm -f $T/_$i.tree
        code=''; ((i++))
        echo
    else
        if [[ '' = "$code" ]]; then
            code="$line"
        else
            code="$code\n$line"
        fi
    fi
done < $file

printf "\e[1;3m%s\e[0m\n" 'Went through all tests!'
