---
title: "Behaviours"
slug: "behaviours"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Defining a behaviour
You can define your own behaviour by adding `-callback` directives in your module.  For example, if modules implementing your behaviour need to have a `foo` function that takes an integer and returns an atom:

    -module(my_behaviour).
    -callback foo(integer()) -> atom().

If you use this behaviour in another module, the compiler will warn if it does not export `foo/1`, and Dialyzer will warn if the types are not correct.  With this module:

    -module(bar).
    -behaviour(my_behaviour).
    -export([foo/1]).
    
    foo([]) ->
        {}.

and running `dialyzer --src bar.erl my_behaviour.erl`, you get these warnings:

<!-- language: none -->

    bar.erl:5: The inferred type for the 1st argument of foo/1 ([]) is not a supertype of integer(), which is expected type for this argument in the callback of the my_behaviour behaviour
    bar.erl:5: The inferred return type of foo/1 ({}) has nothing in common with atom(), which is the expected return type for the callback of my_behaviour behaviour



## Optional callbacks in a custom behaviour
<!-- if version [gte 18.0] -->

By default, any function specified in a `-callback` directive in a behaviour module must be exported by a module that implements that behaviour.  Otherwise, you'll get a compiler warning.

Sometimes, you want a callback function to be optional: the behaviour would use it if present and exported, and otherwise fall back on a default implementation.  To do that, write the `-callback` directive as usual, and then list the callback function in an `-optional_callbacks` directive:

    -callback bar() -> ok.
    -optional_callbacks([bar/0]).

If the module exports `bar/0`, Dialyzer will still check the type spec, but if the function is absent, you won't get a compiler warning.

In Erlang/OTP itself, this is done for the `format_status` callback function in the `gen_server`, `gen_fsm` and `gen_event` behaviours.
<!-- end version if -->


## Using a behaviour
Add a `-behaviour` directive to your module to indicate that it follows a behaviour:

    -behaviour(gen_server).

The American spelling is also accepted:

    -behavior(gen_server).

Now the compiler will give a warning if you've forgotten to implement and export any of the functions required by the behaviour, e.g.:

<!-- language: none -->

    foo.erl:2: Warning: undefined callback function init/1 (behaviour 'gen_server')

