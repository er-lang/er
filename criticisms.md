# Collected criticism against Erlang

Research done on 2013.09.27 with the first Google results for “erlang criticism”.
Some of the presented issues (ressentiments?) have been expressed sometimes 4 years
prior to this work and may have been fixed.

### http://stackoverflow.com/questions/2199368/why-did-you-decide-against-using-erlang
#### Dynamicaly typed
…as opposed to Haskell's famous statically and infered typing system.
#### String
…or lack thereof. No native string type.
#### DB drivers
Tends to have poor (incomplete or unmaintained) database drivers
#### Package manager
It isn't batteries included and doesn't appear to have a community working on correcting this. If it does, it isn't highly visible. Haskell at least has Hackage, and I'd guess that's what has us choosing that language over any other. In Windows environments F# is about to have the ultimate advantage here.
#### It's not C/Java
* Because it looks alien from anyone used to the C family of languages
* Because I wanted to be able to run on the Java Virtual Machine to take advantage of tools I knew and understood (like JConsole) and the years of effort which have gone into JIT and GC.
* Because I didn't want to have to rewrite all the (Java) libraries I've built up over the years.
* Because I have no idea about the Erlang "ecosystem" (database access, configuration, build etc).
#### Awkward commas
I didn't end up using erlang primarily because of syntax.

### http://lambda-the-ultimate.org/node/4350
#### The problem with mutability
The problem with mutability occurs when objects are shared. If mutability is adequately encapsulated (e.g., by not creating aliases to mutable objects), then its not that bad. In this case, encapsulating a mutable object within a process is definitely a good thing, though you might still run into trouble depending how the object is used within the process.
#### Prolog legacy
Well, all true, but having read the blog post in question, his complaint is really not with immutability per se, but rather with the ambiguity of binding and matching in Erlang (due to its roots in Prolog). The example he gives is something like:
(Foo, Bar) <- blah...
... later ...
(Baz, Bar) <- blah...
The third line intends to shadow Bar, but in Erlang it will attempt a match of the previously bound Bar with the right-hand side. A mismatch will result in a run-time error, which is of course a terrible time to find out that you re-used a variable name.
I agree with the blog post that this is probably the wrong default, and I can see why errors of this sort would be easy to make and very frustrating. But in my view it has essentially nothing to do with immutability.
#### Syntax
making the statement separators more uniform, and making the anonymous function syntax a bit more lightweight. (Though Erlang's anonymous functions isn't terribly worse than ML or Lisp: I suppose I've been spoiled by Haskell)

### http://www.unlimitednovelty.com/2011/07/trouble-with-erlang-or-erlang-is-ghetto.html
#### no struct/map data structure
#### Not really open
While Erlang is an open source project, its implementation and release cycle are managed by Ericsson, the company that created it, and Ericsson just doesn't seem to care. I'm not sure what Ericsson's priorities are when it comes to adding features to Erlang, but in my opinion they're doing a worse job of engaging the community than Oracle has been doing with Java.
#### No JIT
Erlang has a "JIT" compiler called HiPE, which is mostly hype. I put JIT in quotes because HiPE is mostly an Erlang-to-native-code compiler with a limited set of backends which does a pretty bad job of optimizing and can't use runtime profiling information to improve the quality of the native code it generates in the way JIT compilers like HotSpot are able to. Calling HiPE a just-in-time compiler is a stretch as it is for most part an ahead-of-time native compiler for Erlang. The quality of native code produced by HiPE can be so poor that it's often outperformed by the userland bytecode interpreter implemented in BEAM.
#### No clever mutability
Immutable state languages force object creation whenever anything changes. This can be partially mitigated by persistent data structures, which are able to share bits and pieces of each other because they're immutable. This works, for example, when attempting to create a sublist that consists of the last N elements of a list. But what if you want the first N elements? You have to make a new list. What if you want elements M..N? You have to make a new list.
In mutable state languages, performance problems can often be mitigated by mutating local (i.e. non-shared) state instead of creating new objects. To give an example from the Ruby language, combining two strings with the + operator, which creates a new string from two old ones, is significantly slower than combining two strings with the concatenating >> operator, which modifies the original string. Mutating state rather than creating new objects means there's fewer objects for the garbage collector to clean up and helps keep your program in-cache on inner loops. If you've seen Cliff Click's crash course on modern hardware, you're probably familiar with the idea that latency from cache misses is quickly becoming the dominating factor in today's software performance. Too much object creation blows the cache.
#### Sometimes wrong/Bad error reporting
{ok, Foo} = do_something(),
...
{ok, Foo} = do_something_else(),
...
view rawgistfile1.erl hosted with ❤ by GitHub
The first pattern matching expression binds the Foo variable to something. In the second case, we've mistakenly forgot Foo was already bound. What's the result?
exception error: no match of right hand side...
We get no compiler warning in this case. This is the type of error you only encounter at runtime. It can lay undetected in your codebase, unless you're writing tests.
#### The standard library is inconsistent, ugly, and riddled with legacy
Should module names in the standard library be plural, like "lists"? Or should they be singular, like "string"? Should we count from 1, as in most of the functions found in things like the lists module, or should we count from 0 like the functions found in the array module? How do I get the length of a list? Is it lists:length/1? No, it's erlang:length/1. How do I get the Nth element of the tuple? Should I look in the tuple module? Wait, there is no tuple module! Instead it's erlang:element/2. How about the length of a tuple? It's erlang:tuple_size/1. Why is the length of a list just "length" whereas the length of a tuple is "tuple_size"? Wouldn't "list_length" be more consistent, as it calls out it works on lists?
When we call erlang:now() to get the current time, it returns {1311,657039,366306}.  What the hell does that mean? It's a tuple with three elements. How could time possible need three elements? A quick look at the documentation reveals that this tuple takes the form {Megaseconds, Seconds, Microseconds}. Separating out Microseconds makes sense... Erlang has no native decimal type so using a float would lose precision. But why split apart Megaseconds and Seconds?
Once upon a time Erlang didn't support integers large enough to store the combination of Megaseconds and Seconds, so they were split apart. The result is a meaningless jumble of three numbers, which you have to run through the confusingly named calendar:now_to_local_time/1 function to get a human meaningful result, which doesn't tell you what time it is now, but instead takes the tuple that erlang:now/0 returns as an argument and will spit back meaningful {Year, Month, Day} and {Hour, Minute, Second} tuples
#### Syntax/Legacy
Try to use "query" as an atom in Erlang, e.g. {query, "SELECT * FROM foobar"}. What happens?
syntax error before: ','
This is because 'query' is a reserved word which was reserved for Mnemosyne queries. Never heard of Mnemosyne? That's because it's an archaic way of querying Erlang's built-in database, Mnesia, and has been replaced with Query List Comprehensions (QLC). However, it remains around for backwards compatibility.
You can't use "query" as a function name. You can't tag a tuple with "query". You can't do anything with "query" except invoke a deprecated legacy API which no one uses anymore.
Even attempting to use "let" in Erlang just yields: syntax error before: 'let'
Once upon a time Erlang was supposed to get let bindings, and the "let" keyword was set aside for this purpose. But much like frames, it never happened. Instead, let is now an unimplemented reserved word which just breaks your programs.
In Clojure, I can write the following: (if false :youll-never-know).  This implicitly returns "nil" because the condition was false. What's the equivalent Erlang?
if
false -> youll_never_know;
true  -> void
end.
view rawgistfile1.erl hosted with ❤ by GitHub
Erlang forces you to specify a clause that always matches regardless of whether you care about the result or not. If no clause matches, you get the amazingly fun "badmatch" exception. In cases where you don't care about the result, you're still forced to add a nonsense clause which returns a void value just to prevent the runtime from raising an exception.
#### No strings
Erlang has no way of differentiating lists of integers that represent strings from lists of integers that are actually lists of integers.
much of the tooling and string functions are designed to work with list-based strings. To leverage these functions, you have to convert a binary to a list before working with it.
Arguing about strings with the old Erlangers is akin to stabbing oneself in the eye with a butter knife. I have re-implented a string object based on binaries, complete with encoding-aware string functions. The response on the list was basically "Why would we need that? A list of integers is fine!" Luddites, really...

### http://rebelscience.blogspot.co.uk/2007/08/seven-deadly-sins-of-erlang.html
#### Does not prevent race conditions
It has no mechanism for automatically enforcing deterministic timing at the operation level. In other words, operations in an Erlang program are neither reactive nor synchronous. This is an absolute must for rock-solid reliability. Note: Synchronous processing is not the same as synchronous messaging.
#### It is implicitly sequential
You would think that your looping process can receive and treat messages in parallel, given the fuss around state. Well think again.
It is explicitly concurrent. Bad for the same reason.
#### Commas & Package manager
Last but not least, Erlang does not have inherent support for plug-compatibility (drag and drop programming) and a searchable/browsable object repository. This is essential to effective and fast software composition and reusability.

### http://damienkatz.net/2008/03/what_sucks_abou.html
#### Syntax
caommas, ifs
#### Warts
strings, records, single assigment even though SSA
#### Code Organization
The only code organization offered is the source file module, there are no classes or namespaces. I'm no OO fanatic (not anymore), but I do see the real value it has: code organization.
Every time time you need to create something resembling a class (like an OTP generic process), you have to create whole Erlang file module, which means a whole new source file with a copyright banner plus the Erlang cruft at the top of each source file, and then it must be added to build system and source control. The extra file creation artificially spreads out the code over the file system, making things harder to follow.
What I wish for is a simple class facility. I don't need inheritance or virtual methods or static checking or monkey patching. I'd just like some encapsulation, the ability to say here is a hunk of data and you can use these methods to taste the tootsie center. That would satisfy about 90% of my unmet project organization needs.
#### Uneven Libraries and Documentation
Most of core Erlang is well documented and designed, but too many of the included modules are buggy, overly complex, poorly documented or all three.
The Inets httpd server we've found incredibly frustrating to use in CouchDB and are discarding it for a 3rd party Erlang HTTP library. The XML processor (Xmerl) is slow, complicated and under documented. Anything in Erlang using a GUI, like the debugger or process monitor, is hideous on Windows and pretty much unusable on OS X. The OTP build and versioning system is complicated and verbose and I still don't understand why it is like it is.
And crufty. I know Erlang has been evolving for real world use for a long time, but that doesn't make the cruftyness it's accumulated over the years smell any better. The coding standards in the core Erlang libraries can differ widely, with different naming, argument ordering and return value conventions. It's tolerable, but it's still there and you must still deal with it.

### http://erlang.org/pipermail/erlang-questions/2001-February/002669.html
#### Syntax
commas
#### No user guards
#### Poor exceptions report
Another vast and poorly lit area is that of exceptions. Today,
there is seldom any way of knowing what sorts of exceptions a function
can throw, and the exceptions are usually uninformative (e.g.,
"badmatch" when you have half a dozen pattern matches in the indicated
function). Thus, _using_ the exit reason to recover from an error is
difficult. In practice, you print, log or ignore an exit today.

### https://medium.com/@elbrujohalcon/there-are-guards-and-guards-71e67d4975d7#.mb942l9z2
#### Syntax
surprising operator precedence
