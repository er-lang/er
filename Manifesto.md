# rlang • A BEAM language featuring an edible syntax
An attempt to resolve the syntax & horrible compile error reports issues.

## Step 1
* A new Grammar, fit for both worlds.
* Compiling to Erlang. clang-like compiler, ie full-featured LLVM backend.
* Unicode source code support.
* Support of Joe's quotes: ‘’, “”

Inspirations: haskell, ruby ?

## Step 2
* Consistent stdlib.
* Binaries are strings by default, not lists.
* Additions to stdlib: UTF-8 strings, …
* Compiling directly to BEAM?

## Step 3
* ???

## Step -1
* World domination.

# Compiling
`make` should do.
## Testing compiled code
`make check`
