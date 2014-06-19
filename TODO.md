# To do

## Grammar
* Devise grammar for `-callback`s
* `export_type`
* `-opaque`
* `|>`-notation for funs & comprehensions
* `-include`, `-include_lib` as #include* ?
* Think about allowing the range operator `..` in pattern matching
* Move `or` & `and` up so that `C == $\n or C == $\t` is `(C == $\n) or (C == $\t)` (this would inhibit usage of `orelse` & `andalso` in cases where they are not needed)
* Refactor `expr` & `last` and such now that all grammar is “secure”

## Parser
* Haskell: [uu-parsinglib](http://hackage.haskell.org/package/uu-parsinglib) or `parsec` + lexer + pretty_printer
