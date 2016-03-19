#!/bin/bash -e -o pipefail

# Looks for code separated by \n\n
# , generates a PNG of the AST
# , checks it against a previously generated PNG.

[[ $# -eq 0 ]] && echo "Usage: [FROM=‹int≥ 1›] [T=‹path test›] $0 ‹a_file.er›" && exit 1

# Enable job control
set -m
#   http://stackoverflow.com/a/12777848/1418165

function P () {
    printf "\e[1;3m%s\e[0m\n" "$1"
}

function Parse () {
    java org.antlr.v4.gui.TestRig Er root -encoding utf8 $*
}

function before_FROM? () {
    [[ $FROM -ne 0 ]] && [[ $1 -lt $FROM ]]
}

function in_EXCEPT? () {
    echo ${EXCEPT[@]/"$1"/"WAS_FOUND"} | grep -q "WAS_FOUND"
}

file="$1"; k=0

T=${T:-'test'}
FROM=${FROM:-1}
EXCEPT=${EXCEPT[@]:-()}

P "Checking '$file'. (stop by removing the generated parser, ^C won't do)."

code=''; i=1
while IFS='' read -r -d $'\n' line
do
    [[ ! -f Er.tokens ]] && P 'No parser found!' && exit 2
    if [[ '' = "$line" ]]; then
        in_EXCEPT?   $i && code='' && ((i++)) && continue
        before_FROM? $i && code='' && ((i++)) && continue
        P "Snippet $i:"
        echo "$code"
        ttree="$T/_$i.tree"
        echo "$code" | Parse -tree > "$ttree"
        if [[ ! -f "$T/$i.tree" ]]; then
            P "Add this new test under '$T/$i.{tree,png}'"
            cat "$ttree"
            echo "$code" | Parse -gui
        else
            diff -u "$T/$i.tree" "$ttree"
	    if [[ $? -ne 0 ]]; then
                P "	Something is wrong with test #$i"
                open "$T/$i.png" || exit 3
                echo "$code" | Parse -gui
            fi
        fi
        rm "$ttree"
        code=''; ((i++))
        echo
    else
        if [[ '' = "$code" ]]
        then code="$line"
        else code="$code"$'\n'"$line"
        fi
    fi
done < "$file"

P "Went through all $(($i-$FROM)) tests!"
