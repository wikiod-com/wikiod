---
title: "Data Types"
slug: "data-types"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Every data type in erlang is called Term. It is a generic name that means *any data type*.

## Numbers
In Erlang, numbers are either integers or floats. Erlang uses arbitrary-precision for integers (bignums), so their values are limited only by the memory size of your system.

    1> 11.
    11
    2> -44.
    -44
    3> 0.1.
    0.1
    4> 5.1e-3.
    0.0051
    5> 5.2e2.
    520.0

Numbers can be used in various bases:

    1> 2#101.
    5
    2> 16#ffff.
    65535

`$`-prefix notation yields the integer value of any ASCII/Unicode character:

    3> $a.
    97
    4> $A.
    65
    5> $2.
    50
    6> $🤖.
    129302



## Binaries and Bitstrings
A binary is a sequence of unsigned 8-bit bytes.
    
    1> <<1,2,3,255>>.
    <<1,2,3,255>>
    2> <<256,257,258>>.
    <<0,1,2>>
    3> <<"hello","world">>.
    <<"helloworld">>

A bitstring is a generalized binary whose length in bits isn't necessarily a multiple of 8.

    1> <<1:1, 0:2, 1:1>>.
    <<9:4>> % 4 bits bitstring


## Maps
A map is an associative array or dictionary composed of (key, value) pairs.

    1> M0 = #{}.
    #{}
    2> M1 = #{ "name" => "john", "age" => "28" }.
    #{"age" => "28","name" => "john"}
    3> M2 = #{ a => {M0, M1} }.
    #{a => {#{},#{"age" => "28","name" => "john"}}}

To update a map :

    1> M = #{ 1 => x }.
    2> M#{ 1 => c }.
    #{1 => c}
    3> M.
    #{1 => x}

Only update some existing key:

    1> M = #{ 1 => a, 2 => b}.
    2> M#{ 1 := c, 2:= d }.
    #{1 => c,2 => d}
    3> M#{ 3 := c }.
    ** exception error: {badkey,3}

Pattern matching:

    1> M = #{ name => "john", age => 28 }.
    2> #{ name := Name, age := Age } = M.
    3> Name.
    "john"
    4> Age.
    28

## Atoms
An atom is an object with a name that is identified only by the name itself.

Atoms are defined in Erlang using atom literals which are either

- an unquoted string that starts with a lowercase letter and contains only letters, digits, underscores or the `@` character, or
- A single quoted string

# Examples

    1> hello.
    hello

    2> hello_world.
    hello_world

    3> world_Hello@.
    world_Hello@

    4> '1234'.     
    '1234'

    5> '!@#$%% ä'.
    '!@#$%% ä'

# Atoms that are used in most Erlang programs

There are some atoms that appear in almost every Erlang program, in particular because of their use in the Standard Library.

- `true` and `false` are the used to denote the respective Boolean values
- `ok` is used usually as a return value of a function that is called only for its effect, or as part of a return value, in both cases to signify a successful execution
- In the same way `error` is used to signify an error condition that doesn't warrant an early return from the upper functions
- `undefined` is usually used as a placeholder for an unspecified value

# Use as tags

`ok` and `error` are quite often used as part of a tuple, in which the first element of the tuple signals success while further elements contain the actual return value or error condition:

    func(Input) ->
        case Input of
            magic_value ->
                {ok, got_it};
            _ ->
                {error, wrong_one}
        end.

    {ok, _} = func(SomeValue).

# Storage
One thing to keep in mind when using atoms is that they are stored in their own global table in memory and this table is not garbage collected, so dynamically creating atoms, in particular when a user can influence the atom name is heavily discouraged.

## Lists
A list in Erlang is a sequence of zero or more Erlang terms, implemented as a singly linked list. Each element in the list can be any type of term (any data type).

    1> [1,2,3].
    [1,2,3]
    2> [wow,1,{a,b}].     
    [wow,1,{a,b}]

The list's head is the first element of the list.
  
The list's tail is the remainder of the list (without the head). It is also a list.  
You can use `hd/1` and `tl/1` or match against `[H|T]` to get the head and tail of the list.

    3> hd([1,2,3]).
    1
    4> tl([1,2,3]).
    [2,3]
    5> [H|T] = [1,2,3].
    [1,2,3]
    6> H.
    1
    7> T.
    [2,3]


# Prepending an element to a list

    8> [new | [1,2,3]].
    [new,1,2,3]

# Concatenating lists

    9> [concat,this] ++ [to,this].
    [concat,this,to,this]

# Strings

In Erlang, strings are not a separate data type: they're just lists of integers representing ASCII or Unicode code points:

    > [97,98,99].
    "abc"
    > [97,98,99] =:= "abc".
    true
    > hd("ABC").
    65

When the Erlang shell is going to print a list, it tries to guess whether you actually meant it to be a string.  You can turn that behaviour off by calling `shell:strings(false)`:

    > [8].
    "\b"
    > shell:strings(false).
    true
    > [8].
    [8]

In the above example, the integer 8 is interpreted as the ASCII control character for backspace, which the shell considers to be a "valid" character in a string.

## Tuples
A tuple is a fixed length ordered sequence of other Erlang terms. Each element in the tuple can be any type of term (any data type).

    1> {1, 2, 3}.
    {1,2,3}
    2> {one, two, three}.
    {one,two,three}
    3> {mix, atom, 123, {<<1,2>>, [list]}}.
    {mix,atom,123,{<<1,2>>,[list]}}



## Funs
Erlang is a functional programming language. One of the features in a function programming language is handling functions as data (functional objects).

* Pass a function as an argument to another function.
* Return function as a result of a function.
* Hold functions in some data structure.

In Erlang those functions are called funs. Funs are anonymous functions.

    1> Fun = fun(X) -> X*X end.
    #Fun<erl_eval.6.52032458>
    2> Fun(5).
    25

Funs may also have several clauses.

    3> AddOrMult = fun(add,X) -> X+X;
    3>                (mul,X) -> X*X 
    3> end.
    #Fun<erl_eval.12.52032458>
    4> AddOrMult(mul,5).
    25
    5> AddOrMult(add,5).
    10

You may also use module functions as funs with the syntax: `fun Module:Function/Arity`.  
For example, lets take the function [`max`](http://erlang.org/doc/man/lists.html#max-1) from `lists` module, which has arity 1.

    6> Max = fun lists:max/1.
    #Fun<lists.max.1>
    7> Max([1,3,5,9,2]). 
    9


## Processes Identifiers (Pid)
Each process in erlang has a process identifier (`Pid`) in this format `<x.x.x>`, `x` being a natural number. Below is an example of a `Pid` 

    <0.1252.0>

`Pid` can be used to send messages to the process using 'bang' (`!`), also `Pid` can be bounded to a variable, both are shown below

    MyProcessId = self().
    MyProcessId ! {"Say Hello"}.

[Read more about creating processes and more in general about processes in erlang][1]


  [1]: https://www.wikiod.com/erlang/processes#Creating Processes

## Bit Syntax: Defaults
Clarification of [Erlang doc][1] on Bit Syntax:


> 4.4 Defaults  
> 
> [Beginning omitted: <<3.14>> isn't even legal syntax.]  
> 
> The default Size depends on the type. For integer it is 8. For float
> it is 64. For binary it is the actual size of the specified binary:
> 
>         1> Bin = << 17/integer, 3.2/float, <<97, 98, 99>>/binary >>. 
>         <<17,64,9,153,153,153,153,153,154,97,98,99>>
>           ^ |<-------------------------->|<------>|
>           |            float=64           binary=24
>       integer=8
>     
>     
>         2> size(Bin). % Returns the number of bytes:
>         12            % 8 bits + 64 bits + 3*8 bits = 96 bits => 96/8 = 12 bytes
> 
> In matching, a binary segment without a Size is only allowed at the
> end of the pattern, and the default Size is the rest of the binary on
> the right hand side of the match:  
> 
>     25> Bin = <<97, 98, 99>>. 
>     <<"abc">>
>     
>     26> << X/integer, Rest/binary >> = Bin. 
>     <<"abc">>
>     
>     27> X. 
>     97
>     
>     28> Rest. 
>     <<"bc">>
> 
> All other segments with type binary in a pattern must specify a Size:
> 
>     12> Bin = <<97, 98, 99, 100>>.         
>     <<"abcd">>
>     
>     13> << B:1/binary, X/integer, Rest/binary >> = Bin. %'unit' defaults to 8 for  
>     <<"abcd">>                    %binary type, total segment size is Size * unit  
>     
>     14> B.
>     <<"a">>
>     
>     15> X.
>     98
>     
>     16> Rest.
>     <<"cd">>
> 
>     17> << B2/binary, X2/integer, Rest2/binary >> = Bin. 
>     * 1: a binary field without size is only allowed at the end of a binary pattern


  [1]: http://erlang.org/doc/programming_examples/bit_syntax.html



