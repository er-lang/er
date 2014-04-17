% Don't forget those:
% http://www.erlang.org/doc/programming_examples/users_guide.html

# Programming Examples User Guide

## Records

---

## Funs

### Example 1 - `map`
If we want to double every element in a list, we could write a function named double:
```haskell
double([H|T]) = [2*H|double(T)]
double([])    = []
```
<!--
This function obviously doubles the argument entered as input as follows:
```> double([1,2,3,4]).```
[2,4,6,8]
-->

We now add the function add_one, which adds one to every element in a list:
```haskell
add_one([H|T]) = [H+1|add_one(T)]
add_one([])    = []
```

These functions, `double` and `add_one`, have a very similar structure.
We can exploit this fact and write a function `map` which expresses this similarity:
```haskell
map(F, [H|T]) = [F(H)|map(F, T)]
map(F, [])    = []
```

We can now express the functions `double` and `add_one` in terms of `map` as follows:
```haskell
double(L)  = map(fun(X) -> 2*X end, L)
add_one(L) = map(fun(X) -> 1 + X end, L)
```

`map(F, List)` is a function which takes a function `F` and a list `L` as arguments
and returns the new list which is obtained by applying `F` to each of the elements in `L`.

The process of abstracting out the common features of a number of different programs
is called procedural abstraction. Procedural abstraction can be used in order
to write several different functions which have a similar structure,
but differ only in some minor detail. This is done as follows:

* write one function which represents the common features of these functions
* parameterize the difference in terms of functions which are passed as arguments to the common function.

### Example 2 -`foreach`
This example illustrates procedural abstraction. Initially, we show the following two examples written as conventional functions:

* all elements of a list are printed onto a stream
* a message is broadcast to a list of processes.

```haskell
print_list(Stream, [H|T]) =
    io:format(Stream, "~p~n", [H])
    print_list(Stream, T)
print_list(Stream, []) =
    true
```

```haskell
broadcast(Msg, [Pid|Pids]) =
    Pid ! Msg
    broadcast(Msg, Pids)
broadcast(_, []) =
    true
```

Both these functions have a very similar structure.
They both iterate over a list doing something to each element in the list.
The "something" has to be carried round as an extra argument to the function which does this.

The function `foreach` expresses this similarity:
```haskell
foreach(F, [H|T]) =
    F(H)
    foreach(F, T)
foreach(F, []) =
    ok
```

Using `foreach`, `print_list` becomes:
```haskell
foreach(fun(H) -> io:format(S, "~p~n",[H]) end, L)
```

`broadcast` becomes:
```haskell
foreach(fun(Pid) -> Pid ! M end, L)
```

`foreach` is evaluated for its side-effect and not its value.
`foreach(Fun, L)` calls `Fun(X)` for each element `X` in `L` and the processing
occurs in the order in which the elements were defined in `L`.
`map` does not define the order in which its elements are processed.

### The Syntax of Funs
Funs are written with the syntax:
```haskell
F = fun (Arg1, Arg2, … ArgN) ->
        …
    end
```
This creates an anonymous function of `N` arguments and binds it to the variable `F`.

If we have already written a function in the same module and wish to pass this function
as an argument, we can use the following syntax:
```haskell
F = fun FunctionName/Arity
```
With this form of function reference, the function which is referred to does not need to be exported from the module.

We can evaluate the fun F with the syntax:
```haskell
F(Arg1, Arg2, ..., ArgN)
```

The following program illustrates the different ways of creating funs:
    ```haskell
    ## module fun_test
    export t1/0 t2/0 double/1 end
    import map/2 from lists

    t1() = map(fun(X) -> 2 * X end, [1,2,3,4,5])

    t2() = map(fun double/1, [1,2,3,4,5])

    double(X) = X * 2
    ```

To check whether a term is a fun, use the test `is_function/1` in a guard. Example:
```haskellf(F, Args) when is_function(F) ->
   apply(F, Args);
   f(N, _) when is_integer(N) ->
      N.
      Funs are a distinct type. The BIFs erlang:fun_info/1,2 can be used to retrieve information about a fun, and the BIF erlang:fun_to_list/1 returns a textual representation of a fun. The check_process_code/2 BIF returns true if the process contains funs that depend on the old version of a module.
---

## List Comprehensions

---

## Bit Syntax

---
