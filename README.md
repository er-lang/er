# [er](https://github.com/fenollp/er) — Erlang's semantics with a simpler syntax

[![Build Status](https://travis-ci.org/fenollp/er.svg?branch=master)](https://travis-ci.org/fenollp/er)


The Er language: an attempt to improve Erlang's syntax quirks & horrible compile-time error reports.


```haskell
zip :: ([A], [B]) -> [{A,B}]
zip ([_|_]=Xs, [_|_]=Ys) ->
    [{X, Y} | X <- Xs
            | Y <- Ys]
# Uses EEP19's comprehension multigenerators
```


```c
module some_attributes
behaviour gen_server

export  start_link/0
export        init/1
       handle_call/3
       handle_cast/2
       handle_info/2
         terminate/2
       code_change/3
record state of {}

start_link () ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], [])

init ([]) ->
    {ok, #{state}}
handle_call (_Request, _From, State) ->
    {reply, ignored, State}
handle_cast (_Msg, State) ->
    {noreply, State}
handle_info (_Info, State) ->
    {noreply, State}
terminate (_Reason, _State) -> ok
code_change (_OldVsn, State, _Extra) ->
    {ok, State}
```


This project **implements a grammar** similar to Erlang's *while much less verbose*. Note: grammar is at worst `LL(*)` (to be determined). For example, Erlang could do without its “weird” line terminators while remaining ambiguity-free as a language (and you don't need to replace them with indentation-based parsing). Lua did it. Go kind of did it. Why wouldn't Erlang do it? also shut off the Erlang's-syntax-is-weird naysaying at the same time

This project is also **a compiler** from said grammar to Core Erlang and has to meet the following constraints:
* instant compile times
* syntax error reports that live up to clang's
* extending compile-passes should be easy
* compiler should be built from grammar file


## Additions | Propositions | Ideas
+ Accepted
    + Keep lowercase as atoms, same for variables
    + Module name is filename (still only one per file) (only ASCII lowercase ∩ {non escape chars})
    + Get rid of `,` `.` `;` as terminators|separators. Guards are be combined with `orelse` & `andalso` (as they often are)
    + Shorter type spec (ie `-type new_name :: old_name`: rm `-type`)
    + `-export([Fun/N|…]).` -> `export Fun/N …`
    +  Rid lexer of legacy rust: `=<<`, `.`, `..`, `...`
    + `#[^\r\n{]*[\r\n]+` comments
    + Support `#warning`, `#ifdef` and the like
    + Use Haskell's way of setting per-file compile options
    + Some Unicode code points can match graphically similar operators, eg. `->` and `↦`
    + Add `..` operator. Meaning different inside `-spec` (does not produce a list there: `0..2 = 0|1|2` (EEP)
    + Add `FUNCTION` macro (same idea as `MODULE`)
    + `:` refers to the function being defined `:(I -1, [V|Acc])` (nice for recursive funs)
    + Change `||` to `|` in *-comprehensions (no ambiguity)
    + Keep `!` for sending messages.
    + No more Erlang's `if` nonsense. Only functional `if…then…else` and `case…of…end` and `cond`.
    + Support new (upcoming EEP) fun syntax: `fun io:format("~p\n")/1` = `fun (X) -> io:format("~p\n", X) end`. Forbid `fun lotsOfArgs(Arg1)/3(Arg4)/12`
    + Support for primes (`'` suffixes) in atom & variable names. Common case of `Var0/Var` and `f/f2`.
    + Support separating digits with `_` as in 1_000 and 100_000.42
    + {}-comprehensions and `[X..Y]` would put forth usage of tuples as immutable arrays as opposed to arrays and bins. Binaries can be thought of mutable when they're >64bits. Bad point: they need specificating type of cell when extracting (though bins of integers don't). Good point: tuples don't need that spec. Bad points: bins are more clever for now so should be faster, and there are nothing in R16B02 helping the use of tuples in that way. Good point: tuples are faster than arrays and they demonstrate the API tuples need.
    + Allow fundef: `‹atom Name› / ‹arity N› = …` for wrappers. Think about guards though. Can combine with EEP on fun.
    + API should put smaller data first for Currying purposes. `[modif(Str) | Str <- Strs] |> fun string:join(“, ”)/1`
    + STDlib funs get mapped without modulename when possible. Error on conflicts. eg: `fun '++'/2` sugars `fun erlang:'++'/2`. No import needed then. Lines shorter. Local defs > STDlib's.
    + Add `rev/1` (for lists) like `hd/1` or `tail/1`. Recursion and linked lists often make use of `lists:reverse/1`.
    + Allow position of generator right of `|` be unimportant.
    + `-compile(export_all).` not a language construct. Only the compile option. (Bad use is bad)
    + Make so that types can be defined in whatever order, like functions.
    + Have a real prepocessor (¬ https://gist.github.com/fenollp/8315947#file-ifdef_in_fun-erl)
    + `{·,1}` be `fun (_1) -> {_1,1} end` like in maths' `||·||` and such.
    + Erlang's `-type` does not accept `when` whereas `-spec` does.
    + Implement EEP19 by transforming to long-arity recursive fun.
    + Use `&&` and `||` for logic. Replace `andalso` and `orelse`
    + `-spec` type-variables defaults to `‹term›`, ‘to mimic Haskell's type parameters’
    + compile-time suggestions on STDlib-found code: `sleep (I) = receive after I -> ok end` <> `timer:sleep(I)`
    + `export start/1,2`, since `start/1,2` is common notation for `start/1, start/2` in the literature
    + One of the jobs of `--static`: http://erlang.org/pipermail/erlang-questions/2010-June/051868.html
    + Get rid of the need of a package manager using Go's package semantics. There's *.app.src for that.
    + Unboxing & inlining such as `compile's inline_list_funcs`. And unboxing of stdlib functions.
    + Prepend `fun ` to º|•-notation.
    + `#include <app1>` = `-include_lib("app1/include/app1.hrl").` If that fails then `-include("include/app1.hrl").`
    + `#include "include/defines.hrl" with ?macro1/0 ?macro2/2` for reading clarity
    + Allow `=` in guards as `=:=`, though warn about `==` & floats when possible
    + Rewrite unused-output *-comprehensions as `foreach/2`. Eg: `_ = [‹body› | ‹arg›<-‹input›]` becomes `lists:foreach(fun (‹arg›) -> ‹body› end, ‹input›)`. So as not to build an output.
    + `--static` should suggest to use `lists:flatmap/2` when it finds `lists:foldr(F,[])/1 ◊ lists:append/2`, …
    + `case Fun of fun M:F/N -> {M,F,N}  _Lambda -> error end` should be allowed given matching exceptions is
* Not rejected yet
    * `=` instead of `->` when defining a function (bad: dramatically slows parser down)
    * Explicit language support for behaviors and other OTP principles
    * A look-alike syntax for named & λ functions. More important: Haskell's λ functions.
    * `?` makes sense for records. Maybe use `?recordX{field_a}` syntax?
    * Define `?R16` and `?R16B01` -style macros for backward-compatible code. Or more like `#if ?ERTS_VSN = R16 …`
    * `--pedantic` requests Dialyzer type-specs on every exported function. Have all functions exported when debugging
    * Have Dialyzer warn on compile (such as to catch `-behavior` issues)
    * Simpler namespacing using Erlang's “library applications”?
    * Somehow bundle all Unicode sugaring as a whole
    * A boolean “matches” operator: `{where,_,_,_,_} =_ element(3,E)`
    * Support new (upcoming EEP) comprehensions: `[… | … <- …]`, `<<… | … <= …>>`, `{… | … <~ …}`.
    * `#{k => v}` and `#k_v{k => v}` are both maps but the second is statically checked.
    * `‹varT Id›{Key}` fetches key `Key` of record|map `Id` defined as a `varT`.
    * `ArgV[2]` | `ArgV[3..]` could be sugar for lists match-assign. Type for lists? Dialyzer? seems no…
    * `MsgA[2]` | `MsgA[3..]` could be sugar for tuple match-assign as well. For maps? no ordering, so no.
    * Note: `s/,//g` prevented by `B = A, (B)`. `B = A (B)` is a function call! An intermediary var would solve that ambiguity.
    * Note: `s/,//g` prevented by `B = A, #{b=B}`. `B = A #{b=B}` is a record creation! New syntax for records hinders this.
    * To counter EEP38-like problems, `--pedantic` would warn about funclauses apart from each other.
    * Add last `receive…end` clause: `_ -> '$Er_ignored_message'` (or warn on debug)
    * Provide a word to common spec `ok | {error, err_type()}` like `?oe(err_type())`. (Just an included macro?)
    * `<:` as the maps generator
    * Automatically-defined maps/records (inlined) getters (… not really Erlangish!)
    * A spec enables optimisation, eg: on calls to polymorphic functions.
    * Look into embedded specs to display terms. Eg: `f :: () -> ‹string› f()=[]`, `> f() #=> ""`
    * Grammar issues regarding record/maps and no `,`?: scan for 'Var#name'.
    * `a | b | c` as match-multiple notation.
- Rejected
    - Differenciate Erlang's multiple meanings of `=` with other shape-like tokens.
    - `'` instead of `$` for chars: `'a, 'b, '\', '\\, '\ `. It'd be nice to allow `$` at ^ of atoms.
    - Possibility following context to use keywords as atoms, like in C++. LALR?
    - Call behaviou?rs interfaces (same concept, helps user to feel she can make her own behaviors)
    - Er's `-spec` also adds the guards to a `spec`-ed function (or a `case…of…end` inside fun's clause). Don't add when guard explicitly written.
    - http://bugs.ruby-lang.org/issues/5054
    - See if `infix{,r,l} ‹0..9› ‹atom›` is possible with a visitor. `(larg0, larg1) atom (rargs) -> … ` could do!
    - Replace `-spec` types (like `any()`, `[integer()]`) with clearer: `‹any›`, `[‹integer›]`
    - Allow `:my_fun()` calls. Sugar for `?MODULE:my_fun()` calls if exported, [local func otherwise](http://erlang.org/pipermail/erlang-questions/2010-June/051772.html) (NO: introduces ambiguity)
    - Change `|` to `:` (but `M:F` hinders that)
    - In Er, *-comprehensions are computed in a non-explicit order. (most of the time are sugar for pmap)
    - `‹funNameAndArgs› ‹guard›* ‹| guarded-clause›* ‹end›` could describe a function. Like Haskell, yes.
    - Support https://en.wikipedia.org/wiki/SI_prefix#List_of_prefixes as: ‹number› ‹SI letter›
    - Allow using `<-` instead of the `receive` keyword. More dynamic impression of an agent receiving messages, Go's channels.
    - `#{k =  v}` and `#k_v{k =  v}` are records. The first is anonymous and maps to just `{v}` (not pretty).
    - Allow `* then ‹fun of arity ≥ 1›` in *-comprehensions such that: `[ x * 10 | x <- lst, x > 2, then sortWith by x ]`.
    - Allow hyphens/others to happen in atom names such as `tree-sum` (NO: there's already underscore)
    - Add Go-channels/ObjC-[]: `!!` sends then immediately receives using unique Ref.
    - Use `finally` in a `receive…end` in place of `after 0`.
    - Modules and files can have different names. As long as at the top of the file it references the right module. This allows for multiple files to be part of the same module. But how to compile modularly?
    - specs can be written with the Curry-arrow notation
    - Exporting `Mod:Fun/A` for wrapper modules. `Fun` locally defined => error.
    - Given `auto-spec-guards addition`, `--f-ascetic-guard-additions`. Don't add guards where Dialyzer says they are not needed.
    - `myfunc :: othermodule:similarfunc/2` ie. use `-spec`s from other modules (DRY). (Makes sense together with wrapper-notation) Enable only for STDlib's exported funcs? Cons: modularity is lost; odd that there is no `fun` keyword; cohabitation with `callback`s is unclear; more importantly, it leads to hiding stuff; maybe same spec, but linking it to another func's is weird as this func would behave differently.
    - Match integer ranges using “naked” `..` (2 cons: matching this is easy enough & naked `..` is worse thanusage of real ranges or disjoint union)

## Step 1
* The whole point is to make Erlang/OTP look postmodern, readable and elegant.
* Programs must be written for people to read, and only incidentally for machines to execute. — Abelson & Sussman, Structure and Interpretation of Computer Programs
* The main point of a high-level programming language is powerful short programs.
* The sole job of a programming language is to bridge the gap between ugly, but efficent, object code, and beautiful source code. -- slidetocode
* An evolution, translation: somewhat backwards compatible since intersection is empty or harmless.
* **Main point**: get rid of `, ; .` mess while **not doing indentation-based parsing**.
* “The alternative to semicolons is making line endings significant.” Nope.
* Unicode source code support.
* This also fixes the things you may have taken for granted if you come from another landscape.
* Support for [Joe's quotes](http://armstrongonsoftware.blogspot.fr/2012/06/what-happened-to-my-quotes.html): `‘’, “”`. `‘’`=`[]` (?) and `“”`=`<<>>`.
* Compile to Core Erlang. clang level of expressivity in errors. Colors available.
* **Lightning-fast compilation times**

## Step 2
* Consistent stdlib. Whole rework of the API. Look at C++'s or Haskell's for containers.
- Binaries are strings by default, not lists.
* Additions to stdlib: UTF-8 strings, …
- Compiling directly to BEAM?
- Fully featured LLVM backend.
* A package manager a-la Homebrew-Hackage-AUR. Community-managed and stuff.
    + A package is identified by its SCM address
    + A package version is a branch or a tag (invalidated when hash of tree changes, due to rebasing for example)
    * A certain and documented project architecture is enforced
    * Build metadata should not live in the project tree
    * Packages (& their versions) live in a flat namespace
    * Packages are compiled once, then symlinked for dev and copied for releases
    * To edit a dep change the symlink to an in-dev version of it
    * During build: dependencies are resolved, packages are fetched and colisions are checked
    * Xref, Dialyzer and a pretty printer should be executing fast and maybe in the background
    * Building updates code in the specified REPLs
    * A central folder holds compiled/fetched packages
    * Updates to the per-project package file (as well as updating deps) wipes generated files and starts anew
