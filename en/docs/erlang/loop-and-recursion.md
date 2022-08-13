---
title: "Loop and Recursion"
slug: "loop-and-recursion"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - function ( list | iolist | tuple ) ->
   function( tail ).

# Why recursive functions?

Erlang is a functional programming language and don't any kind of loop structure. Everything in functional programming is based on data, type and functions. If you want a loop, you need to create a function who call itself.

Traditional `while` or `for` loop in imperative and object oriented language can be represented like that in Erlang:

    loop() ->
      % do something here
      loop().

Good method to understand this concept is to expand all function calls. We'll see that on other examples.

## List
Here the simplest recursive function over [list][1] type. This function only navigate into a list from its start to its end and do nothing more.

    -spec loop(list()) -> ok.
    loop([]) ->
      ok;
    loop([H|T]) ->
      loop(T).

 You can call it like this: 

    loop([1,2,3]). % will return ok.

 Here the recursive function expansion:

    loop([1|2,3]) ->
      loop([2|3]) ->
        loop([3|]) ->
          loop([]) ->
            ok.

# Recursive loop with IO actions

Previous code do nothing, and is pretty useless. So, we will create now a recursive function who execute some actions. This code is similar to [`lists:foreach/2`][2].

    -spec loop(list(), fun()) -> ok.
    loop([], _) ->
      ok;
    loop([H|T], Fun) 
      when is_function(Fun) ->
        Fun(H),
        loop(T, Fun).

You can call it in like this: 

    Fun = fun(X) -> io:format("~p", [X]) end.
    loop([1,2,3]).

Here the recursive function expansion: 

    loop([1|2,3], Fun(1)) ->
      loop([2|3], Fun(2)) ->
        loop([3|], Fun(3)) ->
          loop([], _) ->
            ok.

You can compare with [`lists:foreach/2`][2] output:

    lists:foreach(Fun, [1,2,3]).

# Recursive loop over list returning modified list

Another useful example, similar to [`lists:map/2`][3]. This function will take one list and one anonymous function. Each time a value in list is matched, we apply function on it.

    -spec loop(A :: list(), fun()) -> list().
    loop(List, Fun) 
      when is_list(List), is_function(Fun) ->
        loop(List, Fun, []).
    
    -spec loop(list(), fun(), list()) -> list() | {error, list()}.
    loop([], _, Buffer) 
      when is_list(Buffer) ->
        lists:reverse(Buffer);
    loop([H|T], Fun, Buffer) 
      when is_function(Fun), is_list(Buffer) ->
        BufferReturn = [Fun(H)] ++ Buffer,
        loop(T, Fun, BufferReturn).

You can call it like this:

    Fun(X) -> X+1 end.
    loop([1,2,3], Fun).

Here the recursive function expansion:

    loop([1|2,3], Fun(1), [2]) ->
      loop([2|3], Fun(2), [3,2]) ->
        loop([3|], Fun(3), [4,3,2]) ->
          loop([], _, [4,3,2]) ->
            list:reverse([4,3,2]) ->
              [2,3,4].

This function is also called "tail recursive function", because we use a variable like an accumulator to pass modified data over multiple execution context.


  [1]: http://erlang.org/doc/reference_manual/data_types.html#id70770
  [2]: http://erlang.org/doc/man/lists.html#foreach-2
  [3]: http://erlang.org/doc/man/lists.html#map-2

## Iolist and Bitstring
Like list, simplest function over [iolist and bitstring][1] is:

    -spec loop(iolist()) -> ok | {ok, iolist} .
    loop(<<>>) ->
      ok;
    loop(<<Head, Tail/bitstring>>) ->
      loop(Tail);
    loop(<<Rest/bitstring>>) ->
      {ok, Rest}

You can call it like this:

    loop(<<"abc">>).

Here the recursive function expansion: 

    loop(<<"a"/bitstring, "bc"/bitstring>>) ->
      loop(<<"b"/bitstring, "c"/bitstring>>) ->
        loop(<<"c"/bitstring>>) ->
          loop(<<>>) ->
            ok.

# Recursive function over variable binary size

This code take bitstring and dynamically define binary size of it. So if, if we set a size of `4`, every `4` bits, a data will be matched. This loop do nothing interesting, its just our pillar.

    loop(Bitstring, Size) 
      when is_bitstring(Bitstring), is_integer(Size) ->
        case Bitstring of
          <<>> -> 
            ok;
          <<Head:Size/bitstring,Tail/bitstring>> ->
            loop(Tail, Size);
          <<Rest/bitstring>> ->
            {ok, Rest}
        end.

You can call it like this:

    loop(<<"abc">>, 4).

 Here the recursive function expansion: 

    loop(<<6:4/bitstring, 22, 38, 3:4>>, 4) ->
      loop(<<1:4/bitstring, "bc">>, 4) ->
        loop(<<6:4/bitstring, 38,3:4>>, 4) ->
          loop(<<2:4/bitstring, "c">>, 4) ->
            loop(<<6:4/bitstring, 3:4>>, 4) ->
              loop(<<3:4/bitstring>>, 4) ->
                loop(<<>>, 4) ->
                  ok.

Our bitstring is splitted over 7 patterns. Why? Because by default, Erlang use binary size of `8` bits, if we split it in two, we have `4` bits. Our string is `8*3=24` bits. `24/4=6` patterns. Last pattern is `<<>>`. `loop/2` function is called 7 times.

# recursive function over variable binary size with actions

Now, we can do more interesting thing. This function take one more argument, an anonymous function. Everytime we match a pattern, this one will be passed to it.

    -spec loop(iolist(), integer(), function()) -> ok.
    loop(Bitstring, Size, Fun) ->
      when is_bitstring(Bitstring), is_integer(Size), is_function(Fun) ->
            case Bitstring of
          <<>> -> 
            ok;
          <<Head:Size/bitstring,Tail/bitstring>> ->
            Fun(Head),
            loop(Tail, Size, Fun);
          <<Rest/bitstring>> ->
            Fun(Rest),
            {ok, Rest}
        end.

You can call it like this:

    Fun = fun(X) -> io:format("~p~n", [X]) end.
    loop(<<"abc">>, 4, Fun).

Here the recursive function expansion: 

    loop(<<6:4/bitstring, 22, 38, 3:4>>, 4, Fun(<<6:4>>) ->
      loop(<<1:4/bitstring, "bc">>, 4, Fun(<<1:4>>)) ->
        loop(<<6:4/bitstring, 38,3:4>>, 4, Fun(<<6:4>>)) ->
          loop(<<2:4/bitstring, "c">>, 4, Fun(<<2:4>>)) ->
            loop(<<6:4/bitstring, 3:4>>, 4, Fun(<<6:4>>) ->
              loop(<<3:4/bitstring>>, 4, Fun(<<3:4>>) ->
                loop(<<>>, 4) ->
                  ok.

# Recursive function over bitstring returning modified bitstring

This one is similar to [`lists:map/2`][2] but for bitstring and iolist.

    % public function (interface).
    -spec loop(iolist(), fun()) -> iolist() | {iolist(), iolist()}.
    loop(Bitstring, Fun) ->
      loop(Bitstring, 8, Fun).
    
    % public function (interface).
    -spec loop(iolist(), integer(), fun()) -> iolist() | {iolist(), iolist()}.
    loop(Bitstring, Size, Fun) ->
      loop(Bitstring, Size, Fun, <<>>)
    
    % private function.
    -spec loop(iolist(), integer(), fun(), iolist()) -> iolist() | {iolist(), iolist()}.
    loop(<<>>, _, _, Buffer) ->
      Buffer;
    loop(Bitstring, Size, Fun, Buffer) ->
      when is_bitstring(Bitstring), is_integer(Size), is_function(Fun) ->
        case Bitstring of
          <<>> -> 
            Buffer;
          <<Head:Size/bitstring,Tail/bitstring>> ->
            Data = Fun(Head),
            BufferReturn = <<Buffer/bitstring, Data/bitstring>>,
            loop(Tail, Size, Fun, BufferReturn);
          <<Rest/bitstring>> ->
            {Buffer, Rest}
        end.

This code seems more complexe. Two functions were added: `loop/2` and `loop/3`. These two functions are simple interface to `loop/4`.

You can execute it like this:

    Fun = fun(<<X>>) -> << (X+1) >> end.
    loop(<<"abc">>, Fun).
    % will return <<"bcd">>

    Fun = fun(<<X:4>>) -> << (X+1) >> end.
    loop(<<"abc">>, 4, Fun).
    % will return <<7,2,7,3,7,4>>

    loop(<<"abc">>, 4, Fun, <<>>).
    % will return <<7,2,7,3,7,4>>


  [1]: http://erlang.org/doc/reference_manual/data_types.html#id70915
  [2]: http://erlang.org/doc/man/lists.html#map-2

## Map
[Map][1] in Erlang is equivalent of [hashes][2] in Perl or [dictionaries][3] in Python, its a key/value store. To list every value stored in, you can list every key, and return key/value pair. This first loop give you an idea:

    loop(Map) when is_map(Map) -> 
      Keys = maps:keys(Map),
      loop(Map, Keys).

    loop(_ , []) ->
      ok;
    loop(Map, [Head|Tail]) ->
      Value = maps:get(Head, Map),
      io:format("~p: ~p~n", [Head, Value]),
      loop(Map, Tail).

You can execute it like that:

    Map = #{1 => "one", 2 => "two", 3 => "three"}.
    loop(Map).
    % will return:
    % 1: "one"
    % 2: "two"
    % 3: "three"


  [1]: http://erlang.org/doc/reference_manual/data_types.html#id70076
  [2]: http://perldoc.perl.org/perldata.html
  [3]: https://docs.python.org/2/tutorial/datastructures.html#dictionaries

## Managing State
Recursive function use their states to loop. When you spawn new process, this process will be simply a loop with some defined state.

## Anonymous function
Here 2 examples of recursive [anonymous functions][1] based on previous example. Firstly, simple infinite loop:

    InfiniteLoop = fun 
      R() -> 
        R() end.

Secondly, anonymous function doing loop over list:

    LoopOverList = fun 
      R([]) -> ok;
      R([H|T]) ->
        R(T) end.

These two functions could be rewritten as:

    InfiniteLoop = fun loop/0.

In this case, `loop/0` is a reference to `loop/0` from remarks. Secondly, with little more complex:

    LoopOverLlist = fun loop/2.

Here, `loop/2` is a reference to `loop/2` from list example. These two notations are syntactic sugar.


  [1]: http://erlang.org/doc/reference_manual/data_types.html#id67636

