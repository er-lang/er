# To do

## Grammar
* Allow `module ‹atom›`
* Devise a way of introducing new attributes all matching 1 grammar rule
    * `‹atom AttrName› ‹term Param› (of ‹term RestIfNeeded›)?`
* Devise grammar for `-callback`s
    * `callback`
* `export_type` (prefer less awkward naming)
    * `types`
* `-opaque`
    * `opaque`
* Note: `export`, `callback` and other attribute names needn't be reserved atoms: with PEGs…
* Note: unused common ASCII operators: `%`, `^`, `&`, `~`, `backquote`, `;`, `\`
* `|>`-notation for funs & comprehensions
* `-include`, `-include_lib` as `#include`*
* Think about allowing the range operator `..` in pattern matching
* Refactor `expr` & `last` and such now that all grammar is “secure”
* Lex atoms: `^[0-9][^\s]+`
* Unicode power to numbers? (superscript) `10⁴²`
* Use `#{` instead of just `{` for kvs. This will help for empty kvs. Also easier to read.
* Move `grammar/*` in `.` so that people will actually read it, instead of `criticisms.md`
* Add grammar for preprocessor, if possible. (`#` on col0, scrambled text between parens, …)
* Augment `if…else…end` to `if…elsif…else…end`? Is it really Erlangish? see core of `if…end`
    * `if ‹expr› ‹seqExprs› (elseif ‹expr› ‹seqExprs›)* else ‹seqExprs› end`
* Move `and` & `or` precedences' has high as their exception-gulping counterparts (more power)
    * because `C == $\n or C == $\t` > `(C == $\n) or (C == $\t)`
* Check that `a < x < b` really parses to `x \in ]a;b[`

## Parser
* Haskell: [uu-parsinglib](http://hackage.haskell.org/package/uu-parsinglib) or `parsec` + lexer + pretty_printer
* Or just look for a C backend to ANTLR4. Or an Erlang one. Or an haskell one.

## Documentation
* Put up a pretty and quick one-pager™ on [fenollp.github.io/kju](fenollp.github.io/kju)
* Clean up `README.md` and re-define direction.
* Write a syntax colouring plugin for Pygments or emacs
