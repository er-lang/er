# kju • A BEAM language featuring an edible syntax
An attempt to resolve the syntax & horrible compile error reports issues.

## Step 1
* The whole point is to make Erlang/OTP look postmodern, readable and elegant.
* A new Grammar, fit for both worlds.
* Unicode source code support.
* Support of Joe's quotes: ‘’, “”. ‘’=[] (?) and “”=<<>>.
* Implement ideas:
    * Keep lowercase as atoms, same for variables
    * ‘=’ instead of ‘->’ when defining a function (does that work with funs?)
    * Differenciate Erlang's multiple meanings of ‘=’ with other shape-like tokens.
    * ‘'’ instead of ‘$’ for chars: 'a, 'b, '\', '\\, '\ . It'd be nice to allow ‘$’ at ^ of atoms.
    * Module name is filename (still only one per file). Is facultative.
    * Explicit language support for behaviors and other OTP principles
    * Get rid of ‘,’ ‘.’ ‘;’ with the help of ‘end’ (Keep ‘.’, ‘|’ in mind).
    * Haskell-style type spec (ie ‘-type new_name :: old_name’: rm ‘-type’)
    * -export([Fun|…]). -> export Fun … end
    * Possibility following context to use keywords as atoms, like in C++. LALR?
    * A look-alike syntax for named & λ functions. More important: Haskell's λ functions.
    *  Get rid of legacy rust: ‘=<<’, ‘.’, ‘..’, ‘...’, ‘=<’, …
    * ‘#[^\r\n]*[\r\n]+’ comments; multiline using macros(uh, see other functional p.l.'s m.c.)
    * Can't use ‘#’ for records. ‘?’ makes more sense. Maybe use ‘RecordX{field_a}’ syntax?
    * Support #warning, #ifdef and the like
    * Use Haskell's way of setting per-file compile options
    * Call behaviou?rs interfaces (same concept, helps user to feel she can make her own behaviors)
    * EOL can match ‘;’ or ‘,’. Prefer a syntax that needn't both.
    * Some Unicode code points can match graphically similar operators, eg. ‘->’ and ‘↦’
    * As ‘-callback’ isn't backward-OK, provide different release of Erlang to compile to
    * Define ‘R16’ and ‘R16B01’ -style macros for backward-compatible code
    * Request Dialyzer type-specs on every exported function, implement a --less-pedantic mode, have all functions exported when debugging
    * Have Dialyzer check on compile (such as to catch -behavior issues)
    * KJu's -spec also adds the guards to spec-ed function (or a case…of…end inside fun's clause)
    * http://bugs.ruby-lang.org/issues/5054
    * Simpler namespacing using Erlang's library applications?
    * Somehow bundle all Unicode sugaring as a whole
    * Workout if ‘infix{,r,l} ‹0..9› ‹atom›’ is possible with a visitor. (largs) atom (rargs) -> … . could do!
    * Replace -spec types (like ‘any()’, ‘[integer()]’) with better: ‘‹any›’, ‘[‹integer›]’
    * Add ‘..’ operator. Meaning different inside -spec (does not produce a list there) 0..2 = 0|1|2 ?
    * Add ‘FUNCTION’ macro (same idea as ‘MODULE’)
    * Allow ‘:my_fun()’ calls. Sugar for ‘?MODULE:my_fun()’ calls.
    * Change ‘||’ to ‘|’ in * comprehensions (‘<-’ desambiguates). Maybe also ‘|’ to ‘:’ (M:F calls hinder that)…
* Compile to Erlang/Core Erlang. clang level of expressivity in errors. Color available.

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
