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
```haskell
f(F, Args) when is_function(F) =
    apply(F, Args)
f(N, _) when is_integer(N) =
    N
```

Funs are a distinct type. The BIFs `erlang:fun_info/1,2` can be used to retrieve
information about a fun, and the BIF `erlang:fun_to_list/1` returns a textual
representation of a fun. The `check_process_code/2` BIF returns `true` if the process
contains funs that depend on the old version of a module.

### Variable Bindings Within a Fun
The scope rules for variables which occur in funs are as follows:

* All variables which occur in the head of a fun are assumed to be "fresh" variables.
* Variables which are defined before the fun, and which occur in function calls or
guard tests within the fun, have the values they had outside the fun.
* No variables may be exported from a fun.

The following examples illustrate these rules:
```haskell
print_list(File, List) =
    {ok, Stream} = file:open(File, write)
    foreach(fun(X) -> io:format(Stream,"~p~n",[X]) end, List)
    file:close(Stream)
```

In the above example, the variable `X` which is defined in the head of the fun
is a new variable. The value of the variable `Stream` which is used within the fun
gets its value from the `file:open` line.

Since any variable which occurs in the head of a fun is considered a new variable
it would be equally valid to write:
```haskell
print_list(File, List) =
    {ok, Stream} = file:open(File, write)
    foreach(fun(File) -> 
                io:format(Stream,"~p~n",[File]) 
            end, List)
    file:close(Stream)
```

<!--- ... --->

### Funs and the `lists` Module
The following examples show a dialogue with the Erlang shell.
All the higher order functions discussed are exported from the module `lists`.

### `map`
```haskell
map(F, [H|T]) = [F(H)|map(F, T)]
map(F, [])    = []
```
<!--- ... --->

### `any`
```haskell
any(Pred, [H|T]) =
    if Pred(H) then true
    else any(Pred, T) end
any(Pred, []) 
    false
```
<!--- ... --->

### `all`
```haskell
all(Pred, [H|T]) =
    if Pred(H) then all(Pred, T)
    else false
    end
all(Pred, []) =
    true
```
<!--- ... --->

### `foreach`
```haskell
foreach(F, [H|T]) =
    F(H)
    foreach(F, T)
foreach(F, []) =
    ok
```
<!--- ... --->

### `foldl`
```haskell
foldl(F, Accu, [Hd|Tail]) =
    foldl(F, F(Hd, Accu), Tail)
foldl(F, Accu, []) = Accu
```
<!--- ... --->

### `mapfoldl`
```haskell
mapfoldl(F, Accu0, [Hd|Tail]) =
    {R,Accu1} = F(Hd, Accu0)
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail)
    {[R|Rs], Accu2}
mapfoldl(F, Accu, []) = {[], Accu}
```
<!--- ... --->

### `filter`
```haskell
filter(F, [H|T]) =
    if F(H) then [H|filter(F, T)]
            else    filter(F, T)
    end
filter(F, []) = []
```
<!--- ... --->

### `takewhile`
```haskell
takewhile(Pred, [H|T]) =
    if Pred(H)
    then [H|takewhile(Pred, T)]
    else []
    end
takewhile(Pred, []) =
    []
```
<!--- ... --->

### `dropwhile`
```haskell
dropwhile(Pred, [H|T]) =
    if Pred(H) then
        dropwhile(Pred, T)
    else [H|T]
    end
dropwhile(Pred, []) =
    []
```
<!--- ... --->

### `splitwith`
```haskell
splitwith(Pred, L) =
    splitwith(Pred, L, [])

splitwith(Pred, [H|T], L) =
    if Pred(H) then
        splitwith(Pred, T, [H|L])
    else
        {reverse(L), [H|T]}
    end
splitwith(Pred, [], L) =
    {reverse(L), []}
```
<!--- ... --->



---

## List Comprehensions
<!--- ... --->
### Quick Sort
The well known quick sort routine can be written as follows:
```haskell
sort([Pivot|T]) =
    sort([ X | X <- T  X < Pivot]) ++
    [Pivot] ++
    sort([ X | X <- T  X ≥ Pivot])
sort([]) = []
```
The expression `[X | X <- T  X < Pivot]` is the list of all elements in `T`,
which are less than `Pivot`.

To sort a list, we isolate the first element in the list and split the list into two sub-lists.
The first sub-list contains all elements which are smaller than the first element in the list,
the second contains all elements which are greater than or equal to the first element in the list.
We then sort the sub-lists and combine the results.

### Permutations
The following example generates all permutations of the elements in a list:
```haskell
perms([]) = [[]]
perms(L)  = [[H|T] | H <- L  T <- perms(L--[H])]
```
We take take `H` from `L` in all possible ways.
The result is the set of all lists `[H|T]`, where `T` is the set of all possible
permutations of `L` with `H` removed.
<!--- ... --->

### Simplifications with List Comprehensions
As an example, list comprehensions can be used to simplify some of the functions in `lists.erl`:
```haskell
append(L)       = [    X  | L1 <- L  X <- L1]
map(Fun, L)     = [Fun(X) |  X <- L         ]
filter(Pred, L) = [    X  |  X <- L  Pred(X)]
```

### Variable Bindings in List Comprehensions
---

## Bit Syntax

---
