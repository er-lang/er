# rlang • A BEAM language featuring an edible syntax
An attempt to resolve the syntax & horrible compile error reports issues.

## Step 1
* A new Grammar, fit for both worlds.
* Unicode source code support.
* Support of Joe's quotes: ‘’, “”
* Implement ideas:
    * Keep lowercase as atoms, same for variables
    * ‘=’ instead of ‘->’ ; ie add context not to confuse w/ pattern matching?
    * ‘'’ instead of ‘$’ for chars: 'a, 'b, '\', '\\, '\ .
    * Module name is filename
    * Explicit language support for behaviors and other OTP principles
    * Get rid of ‘,’ ‘.’ ‘;’ with maybe the help of ‘end’. Ruby?
    * Haskell-style type spec (ie ‘-type new_name :: old_name’: rm ‘-type’)
    * -export([Fun|…]). -> export Fun … end
    * Possibility following context to use keywords as atoms, like in C++.
    * A look-alike syntax for named & λ functions
    * Get rid of legacy rust: ‘=<<’, ‘.’, ‘..’, ‘...’, …
    * ‘#[^\r\n]*[\r\n]+’ comments; multiline using macros(uh, see other functional p.l.'s m.c.)
    * There is no need to put a ‘#’ char when using records
    * Throw a warning when exported functions aren't spec-ed
    * Support #warning, #ifdef and the like
    * Use Haskell's way of setting per-file compile options
    * Call behaviou?rs interfaces (same concept, helps user to feel she can make her own behaviors)
    * EOL can match ‘;’ or ‘,’
    * Some Unicode code points can match graphically similar operators, eg. ‘->’ and ‘↦’
* Compile to Erlang. clang level of expressivity in errors.

Inspirations: haskell, ruby ?

## Step 2
* Consistent stdlib.
* Binaries are strings by default, not lists.
* Additions to stdlib: UTF-8 strings, …
* Compiling directly to BEAM?
* Fully featured LLVM backend.

## Step 3
* ???

## Step -1
* World domination.

# Compiling
`make` should do.
## Testing compiled code
`make check`
