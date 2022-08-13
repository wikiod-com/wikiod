---
title: "Getting started with Erlang Language"
slug: "getting-started-with-erlang-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
There are two things you will need to know when writing a "hello world" application in Erlang:
1. The source code is written in the *erlang programming language* using the text editor of your choice
2. The application is then executed in the *erlang virtual machine*. In this example we will interact with the erlang VM thorugh the erlang shell.

First the application source code:
- 

Create a new file `hello.erl` containing the following:

    -module(hello).
    -export([hello_world/0]).

    hello_world() ->
      io:format("Hello, World!~n", []).

Let's have a quick look at what this means:
- `-module(hello).` All erlang functions exists inside a *module*. Modules are then used to build applications, which are a collection of modules. This first line is to identify this module, namely *hello*. Modules can be compared to Java's *packages*
- `-export([hello_world/0]).` Tells the compiler which functions to make "public" (when compared to OO languages), and the *arity* of the relevant function. The arity is the number of arguments the function takes. Since in erlang a function with 1 argument is seen as a different function than one with 2 arguments even though the name may be exactly the same. Ie, `hello_world/0` is a completely different function than `hello_world/1` for example.
- `hello_world()` This is the name of the function. The `->` indicates the transitioning to the implementation (body) of the function. This can be read as "hello_world() is defined as ...". Take note that `hello_world()` (no arguments) is identified by `hello_world/0` in the VM, and `hello_world(Some_Arg)` as `hello_world/1`.
- `io:format("Hello, World!~n", [])` From module `io`, the function `format/2` function is called, which is the function for standard output. `~n` is a format specifier that means print a new line. The `[]` is a list of variables to print indicated by format specifiers in the output string, which is in this case nothing.
- All erlang statements must end with a `.` (dot).

In Erlang, the result of the last statement in a function is returned.

Now, let's run our application:
-

Start the erlang shell from same directory as the file `hello.erl` file:

`$ erl`

You should get a prompt that looks something like this (your version may be different):

```
Eshell V8.0  (abort with ^G)
1>
```

Now enter the following commands:

```
1> c(hello).
{ok,hello}
2> hello:hello_world().
Hello, World!
ok
```

Let's go through each line one by one:

* `c(hello)` -  this command calls the function `c` on an atom `hello`. This effectively tells Erlang to find the file `hello.erl`, compile it into a module (a file named `hello.beam` will be generated in the directory) and load it into the environment. 
* `{ok, hello}` - this is the result of calling the function `c` above. It is a tuple containing an atom `ok` and an atom `hello`. Erlang functions usually return either `{ok, Something}` or `{error, Reason}`.
* `hello:hello_world()` - this calls a function `hello_world()` from the module `hello`.
* `Hello, World!` - this is what our function prints.
* `ok` - this is what our function returned. Since Erlang is a functional programming language, every function returns _something_. In our case, even though we didn't return anything in `hello_world()`, the last call in that function was to `io:format(...)` and that function returned `ok`, which is in turn what our function returned.

## List Comprehension
List comprehensions are a syntactic construct to create a list based on existing lists.  
In erlang a list comprehension has the form ```[Expr || Qualifier1, ..., QualifierN]```.  
Where qualifiers are either generators ```Pattern <- ListExpr``` or filter like ```integer(X)``` evaluating to either ```true``` or ```false```.

 The following example shows a list comprehension with one generator and two filters.

    [X || X <- [1,2,a,3,4,b,5,6], integer(X), X > 3].

The result is a list containing only integers greater than 3.

    [4,5,6]

## Modules
An erlang module is a file with couple of functions grouped together. This file usually has `.erl` extension.

A "Hello World" module with name `hello.erl` is shown below

    -module(hello).
    -export([hello_world/0]).
    
    hello_world() ->
      io:format("Hello, World!~n", []).

In the file, it is required to declare the module name. As shown before in line 1.  The module name and file name before `.erl` extension must be same.

## Function
Function is a set of instructions, which are grouped together. These grouped instructions together perform certain task. In erlang, all the functions will return a value when they are called.

Below is an example of a function that adds two numbers

    add(X, Y)-> X + Y.

This function performs an add operation with X and Y values and returns the result. Function can be used as below

    add(2,5).

Function declarations can consist of multiple clauses, separated by a semicolon. The Arguments in each of these clauses are evaluated by pattern matching. The following function will return 'tuple' if the Argument is a tuple in the Form: {test, X} where X can be any value. It will return 'list', if the Argument is a list of the length 2 in the form ["test", X], and It will return '{error, "Reason"}' in any other case:

    function({test, X}) -> tuple;
    function(["test", X]) -> list;
    function(_) -> {error, "Reason"}.

If the argument is not a tuple, the second clause will be evaluated. If the argument is not a list, the third clause will be evaluated.

Function declarations can consist of so called 'Guards' or 'Guard Sequences'. These Guards are expressions that limit the evaluation of a function. A function with Guards is only executed, when all Guard Expressions yield a true value. Multiple Guards can be seperated by a semicolon.

    function_name(Argument) when Guard1; Guard2; ... GuardN -> (...).

The function 'function_name' will only be evaluated, when the Guard Sequence is true. The follwing function will return true only if the argument `X` is in the proper range (0..15): 

    in_range(X) when X>=0; X<16 -> true;
    in_range(_) -> false.



## Starting and Stopping the Erlang Shell
**Starting the Erlang shell**

On a UNIX system you start the Erlang shell from a command prompt with the command `erl`

Example:

    $ erl
    Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.0  (abort with ^G)
    1> 


The text that shows when you start the shell tells you information about which version of Erlang you are running as well as other useful information about the erlang system.

To start the shell on Windows you click the Erlang-icon in the windows start menu.

**Stopping the Erlang shell**

For a controlled exit of the erlang shell you type:

    Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.0  (abort with ^G)
    1> q().

You can also exit the Erlang shell by pressing Ctrl+C on UNIX systems or Ctrl+Break on Windows, which brings you to the following prompt:

    Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.0  (abort with ^G)
    1> 
    BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
           (v)ersion (k)ill (D)b-tables (d)istribution

If you then press a (for abort) you will exit the shell directly.

Other ways of exiting the erlang shell are: `init:stop()` which does the same thing as `q()` or `erlang:halt()`.




## Pattern Matching
One of the most common operations in erlang is pattern matching. It is used when assigning a value to a variable, in function declarations and in control-flow structures like `case` and `receive` statements. A pattern matching operation needs at least 2 parts: a pattern, and a term against wich the pattern is matched.

A variable assignment in erlang looks like this:

    X = 2.

In most programming language the semantics of this operation is straightforward: Bind a value (`2`) to a name of your choice (the variable -- in this case `X`). Erlang has a slightly different approach: Match the pattern on the left hand side (`X`) to the term on the right hand side (`2`). In this case, the effect is the same: the variable `X` is now bound to the value `2`. However, with pattern matching you are able to perform more structured assignments.

    {Type, Meta, Doc} = {document, {author, "Alice"}, {text, "Lorem Ipsum"}}.

This matching operation is performed, by analyzing the structure of the right hand side term, and applying all variables on the left hand side to the appropriate values of the term, so that the left side equals the right side. In this example `Type` is bound to the term: `document`, `Meta` to  `{author, "Alice"}` and `Doc` to `{text, "Lorem Ipsum"}`. In this particular example the variables: `Type`, `Meta` and `Doc` are assumed to be *unbound*, so that each variable can be used.

Pattern matchings can also be built, using bound variables.

    Identifier = error.

The variable `Identifier` is now bound to the value `error`. The following pattern matching operation works, because the structure matches, and the bound variable `Identifier` has the same value like the appropriate right hand side part of the term.

    {Identifier, Reason} = {error, "Database connection timed out."}.

A pattern matching operation fails, when there is a mismatch between right hand side term and left hand side pattern. The following match will fail, because `Identifier` is bound to the value `error`, wich has no appropriate expression on the right hand side term.

    {Identifier, Reason} = {fail, "Database connection timed out."}.
    > ** exception error: no match of right hand side value {fail,"Database ..."}


