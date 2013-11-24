# kju • [A BEAM language featuring an edible syntax](https://github.com/fenollp/kju)
An attempt to resolve the syntax & horrible compile error reports issues.

    kju K-ju Kaiju
    Le monstre magnifique
    The magnificent monster

Maybe Kim Jong-Un in that it is a merciless tool.

## Step 1
* The whole point is to make Erlang/OTP look postmodern, readable and elegant.
* Programs must be written for people to read, and only incidentally for machines to execute. — Abelson & Sussman, Structure and Interpretation of Computer Programs
* The main point of a high-level programming language is powerful short programs.
* The sole job of a programming language is to bridge the gap between ugly, but efficent, object code, and beautiful source code. -- slidetocode
* A new Grammar, fit for both worlds.
* An evolution, translation: somewhat backwards compatible since intersection is empty or harmless.
* Main point: get rid of ‘, ; . end’ mess while not doing indentation-based parsing.
* Unicode source code support.
* This also fixes the things you may have taken for granted if you come from another landscape.
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
    * KJu's -spec also adds the guards to spec-ed function (or a case…of…end inside fun's clause) Eg: is_string/1 “guard”.
    * http://bugs.ruby-lang.org/issues/5054
    * Simpler namespacing using Erlang's “library applications”?
    * Somehow bundle all Unicode sugaring as a whole
    * See if ‘infix{,r,l} ‹0..9› ‹atom›’ is possible with a visitor. (larg0, larg1) atom (rargs) -> … . could do!
    * Replace -spec types (like ‘any()’, ‘[integer()]’) with better: ‘‹any›’, ‘[‹integer›]’
    * Add ‘..’ operator. Meaning different inside -spec (does not produce a list there) 0..2 = 0|1|2 ?
    * Allow ‘:my_fun()’ calls. Sugar for ‘?MODULE:my_fun()’ calls.
    * Add ‘FUNCTION’ macro (same idea as ‘MODULE’). Also ‘:’ makes a lot of sense befor parenthesis: ‘:(I -1, [V|Acc])’
    * Change ‘||’ to ‘|’ in *-comprehensions (no ambiguity). Maybe also ‘|’ to ‘:’ (‘M:F’ hinders that)…
    * In kju *-comprehensions are computed in a non-explicit order. (most of the time are sugar for pmap)
    * Keep ‘!’ for sending messages.
    * No more Erlang's ‘if’ nonsense. Only functional if…then…else…end and case…of…end.
    * Every Unicode (non-standard-keyboard-writable) thing is ASCII-compatible with something.
    * Extend Erlang's equality operators to work with underscores: ‘{where,_,_,_,_} =:= element(3,E)’
    * Support new (upcoming EEP) fun syntax: ‘fun io:format("~p\n")/1’ = ‘fun (X) -> io:format("~p\n", X) end’. May allow ‘fun lotsOfArgs(Arg1)/3(Arg4)/12’
    * Support new (upcoming EEP) comprehensions: ‘[… | … <- …]’, ‘<<… | … <= …>>’, ‘{… | … <~ …}’.
    * Support primes names (‘'’ suffixes) for atoms & variables. Common case of Var0/Var and f/f2.
    * ‹funNameAndArgs› ‹guard›* ‹| guarded-clause›* ‹end› could describe a function. Like Haskell, yes.
    * Type specs = its own DSL. Needs variables, atoms, type names, ranges… Haskells' = very nice.
    * Support https://en.wikipedia.org/wiki/SI_prefix#List_of_prefixes as: ‹number› ‹SI letter›
    * Support separating digits with ‘_’ as in 1_000 and 100_000.42
    * MAY allow using ‘<-’ instead of the ‘receive’ keyword. This gives a more dynamic impression of an agent receiving messages.
    * Add a keyword that refers to the name of the function being defined. Maybe a macro. I think ‘fun()’ is a good compromise.
    * ‘#{k => v}’ and ‘#k_v{k => v}’ are both maps but the second is statically checked.
    * ‘#{k =  v}’ and ‘#k_v{k =  v}’ are records. The first is anonymous and maps to just ‘{v}’ (not pretty).
    * Current Erlang's records/maps syntax prevent removal of commas in funs' exprs.
    * Besides, Erlang allows just-variables in fun-exprs, which is ignored, funny and sad.
    * Allow fundef: “‹atom Name› ‘/’ ‹arity N› ‘=’ …” for wrappers. Think about guards though. Can combine with EEP on fun.
    * Allow * ‘then ‹fun of arity ≥ 1›’ in *-comprehensions such that: ‘[ x * 10 | x <- lst, x > 2, then sortWith by x ]’.
    * API should put smaller data first for Currying purposes. ‘[modif(Str) || Str <- Strs, then fun string:join(“, ”)/1]’ ‘[F(X) || X<-Xs, then '++'/2]’.
    * Should be possible to deftype a -spec and then just ‘myfun :: ‹funspec1›. myfun (…) = ….’
    * Better lexing overall: allow hyphens to happen in atom names such as ‘tree-sum’
    * Add rev/1 (for lists) like hd/1 or tail/1. Recursion and linked lists often make use of lists:reverse/1.
    * May allow position of generator be swapped regarding ‘|’ (or ‘||’)
    * Add Go-channels/ObjC-[]: a new operator that sends then immediately receives using unique Ref.
    * ‘-compile(export_all).’ not a language construct. Only the compile option. (Bad use is bad)
    * To counter EEP38-like problems, --pedantic would warn about funclauses appart from each other.
    * ‘--static’ MAY suggest to use lists:flatmap/2 when it finds lists:foldr(F,[])/1 ◊ lists:append/2, …
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
