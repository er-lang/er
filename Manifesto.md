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
    * Module name is filename (still only one per file)
    * Explicit language support for behaviors and other OTP principles
    * Get rid of ‘,’ ‘.’ ‘;’ with the help of ‘end’.
    * Haskell-style type spec (ie ‘-type new_name :: old_name’: rm ‘-type’)
    * -export([Fun|…]). -> export Fun … end
    * Possibility following context to use keywords as atoms, like in C++. LALR?
    * A look-alike syntax for named & λ functions
    * Get rid of legacy rust: ‘=<<’, ‘.’, ‘..’, ‘...’, …
    * ‘#[^\r\n]*[\r\n]+’ comments; multiline using macros(uh, see other functional p.l.'s m.c.)
    * Can't use ‘#’ for records. ‘?’ makes more sense. Maybe use ‘RecordX{field_a}’ syntax?
    * Support #warning, #ifdef and the like
    * Use Haskell's way of setting per-file compile options
    * Call behaviou?rs interfaces (same concept, helps user to feel she can make her own behaviors)
    * EOL can match ‘;’ or ‘,’
    * Some Unicode code points can match graphically similar operators, eg. ‘->’ and ‘↦’
    * As ‘-callback’ isn't backward-OK, provide different release of Erlang to compile to
    * Define ‘R16’ and ‘R16B01’ -style macros for backward-compatible code
    * Request Dialyzer type-specs on every exported function, implement a --less-pedantic mode, have all functions exported when debugging
    * Have Dialyzer check on compile (such as to catch -behavior issues)
    * Rlang's -spec also adds the guards to spec-ed function
    * http://bugs.ruby-lang.org/issues/5054
    * Simpler namespacing using Erlang's library applications?
* Compile to Erlang/Core Erlang. clang level of expressivity in errors.

## Step 2
* Consistent stdlib. Whole rework of the API. Look at C++'s or Haskell's for containers.
* Binaries are strings by default, not lists.
* Additions to stdlib: UTF-8 strings, …
* Compiling directly to BEAM?
* Fully featured LLVM backend.
* A package manager a-la Homebrew-Hackage-AUR. Community-managed and stuff.

## Step 3
* ???

## Step -1
* World domination.

# Compiling
`make` should do.
## Testing compiled code
`make check`
