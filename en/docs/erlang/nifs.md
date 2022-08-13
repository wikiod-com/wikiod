---
title: "NIFs"
slug: "nifs"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Example: current UNIX time
Here is a very simple example to illustrate how to write a NIF.

Directory structure:

    nif_test
    ├── c_src
    │   ├── Makefile
    │   └── nif_test.c
    ├── rebar.config
    └── src
        ├── nif_test.app.src
        └── nif_test.erl

This structure can be easily initialized using Rebar3:

    $ rebar3 new lib nif_test && cd nif_test && rebar3 new cmake

Contents of `nif_test.c`:

    #include "erl_nif.h"
    #include "time.h"
    
    static ERL_NIF_TERM now(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
            return enif_make_int(env, time(0));
    }
    
    static ErlNifFunc nif_funcs[] = {
            {"now", 0, now}
    };
    
    ERL_NIF_INIT(nif_test,nif_funcs,NULL,NULL,NULL,NULL);

Contents of `nif_test.erl`:

    -module(nif_test).
    -on_load(init/0).
    -export([now/0]).
    
    -define(APPNAME, nif_test).
    -define(LIBNAME, nif_test).
    
    %%====================================================================
    %% API functions
    %%====================================================================

    now() -> nif_not_loaded.
    
    %%====================================================================
    %% Internal functions
    %%====================================================================
    
    init() ->
        SoName = case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true -> filename:join(["..", priv, ?LIBNAME]);
                    _ -> filename:join([priv, ?LIBNAME])
                end;
            Dir -> filename:join(Dir, ?LIBNAME)
        end,
        erlang:load_nif(SoName, 0).
    
Contents of `rebar.config`:

    {erl_opts, [debug_info]}.
    {deps, []}.
    
    {pre_hooks, [
      {"(linux|darwin|solaris)", compile, "make -C c_src"},
      {"(freebsd)", compile, "gmake -C c_src"}
    ]}.
    {post_hooks, [
      {"(linux|darwin|solaris)", clean, "make -C c_src clean"},
      {"(freebsd)", clean, "gmake -C c_src clean"}
    ]}.

Now you can run the example:

    $ rebar3 shell                                                                                                                                        
    ===> Verifying dependencies...
    ===> Compiling nif_test
    make: Entering directory '/home/vschroeder/Projects/nif_test/c_src'
    cc -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes -fPIC -I /usr/local/lib/erlang/erts-7.3.1/include/ -I /usr/local/lib/erlang/lib/erl_interface-3.8.2/include  -c -o /home/vschroeder/Projects/nif_test/c_src/nif_test.o /home/vschroeder/Projects/nif_test/c_src/nif_test.c
    cc /home/vschroeder/Projects/nif_test/c_src/nif_test.o -shared -L /usr/local/lib/erlang/lib/erl_interface-3.8.2/lib -lerl_interface -lei -o /home/vschroeder/Projects/nif_test/c_src/../priv/nif_test.so
    make: Leaving directory '/home/vschroeder/Projects/nif_test/c_src'
    Erlang/OTP 18 [erts-7.3.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V7.3.1  (abort with ^G)
    1> nif_test:now().
    1469732239
    2> nif_test:now().
    1469732264
    3> 

## Definition
Official documentation: http://erlang.org/doc/tutorial/nif.html

NIFs were introduced in Erlang/OTP R13B03 as an experimental feature. The purpose is to allow calling C-code from inside Erlang code.

NIFs are implemented in C instead of Erlang, but they appear as any other functions in the scope of Erlang code as they belong to the module where the include happened. NIF libraries are linked on compilation and loaded in runtime.

Because NIF libraries are dynamically linked into the emulator process, they are fast, but also dangerous, because crashing in a NIF brings the emulator down too.

## Erlang C API (C to Erlang)
**Official documentation**: http://erlang.org/doc/man/erl_nif.html

The most important structs, types and macros of the Erlang C API are the following:

- `ERL_NIF_TERM`: the type for Erlang terms. This is the return type that NIF functions must follow.
- `ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)`: This is the macro that actually creates the NIFs defined in a certain C file. It must be evaluated in the global scope. Normally it will be the last line in the C file.
- `ErlNifFunc`: the type with which each NIF is passed to `ERL_NIF_INIT` to be exported. This struct is composed of name, arity, a poiter to the C function and flags. An array of this type with all NIF definitions should be created to be passed to `ERL_NIF_INIT`.
- `ErlNifEnv`: the Erlang environment where the NIF is being executed. It's mandatory to pass the environment as the first argument for every NIF. This type is opaque and can only be manipulated using the functions that the Erlang C API offers.

