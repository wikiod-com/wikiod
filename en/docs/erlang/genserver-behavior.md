---
title: "gen_server behavior"
slug: "gen_server-behavior"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

`gen_server` is an important feature of Erlang, and require some prerequisite to understand every aspect of this functionality:

 * [Loop, recursion and state][1]
 * [Spawning processes][2]
 * [Message passing][3]
 * [OTP principles][4]

A good way to learn more about a feature in Erlang is to directly read the source code from [official github repository][5]. `gen_server` behavior embed lot of useful information and interesting structure in its core.

`gen_server` is defined in [`gen_server.erl`][6] and its associated documentation can be find in [`stdlib` Erlang documentation][7]. `gen_server` is an OTP feature and more information can be also found in [OTP Design Principles and User's Guide][8].

Frank Hebert give you also another good introduction to `gen_server` from its free online book [Learn You Some Erlang for great good!][9]

Official documentation for `gen_server` callback:

 * [`code_change/3`][10]
 * [`handle_call/3`][11]
 * [`handle_cast/2`][12]
 * [`handle_info/2`][13]
 * [`init/1`][14]
 * [`terminate/2`][15]


  [1]: http://erlang.org/doc/efficiency_guide/listHandling.html
  [2]: http://erlang.org/doc/reference_manual/processes.html
  [3]: http://erlang.org/doc/getting_started/conc_prog.html#id68696
  [4]: http://erlang.org/doc/design_principles/des_princ.html
  [5]: https://github.com/erlang/otp
  [6]: https://github.com/erlang/otp/blob/maint/lib/stdlib/src/gen_server.erl
  [7]: http://erlang.org/doc/man/gen_server.html
  [8]: http://erlang.org/doc/design_principles/gen_server_concepts.html
  [9]: http://learnyousomeerlang.com/clients-and-servers
  [10]: http://erlang.org/doc/man/gen_server.html#Module:code_change-3
  [11]: http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
  [12]: http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
  [13]: http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
  [14]: http://erlang.org/doc/man/gen_server.html#Module:init-1
  [15]: http://erlang.org/doc/man/gen_server.html#Module:terminate-2

## Greeter Service


## Using gen_server behavior
A `gen_server` is a specific finite state machine working like a server. `gen_server` can handle different type of event:

 * synchronous request with `handle_call`
 * asynchronous request with `handle_cast`
 * other message (not defined in OTP specification) with `handle_info`

Synchronous and asynchronous message are specified in OTP and are simple tagged tuples with any kind of data on it.

A simple `gen_server` is defined like this:

    -module(simple_gen_server).
    -behaviour(gen_server).
    -export([start_link/0]).
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
             terminate/2, code_change/3]).
    
    start_link() ->
        Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
        io:format("start_link: ~p~n", [Return]),
        Return.
    
    init([]) ->
        State = [],
        Return = {ok, State},
        io:format("init: ~p~n", [State]),
        Return.
    
    handle_call(_Request, _From, State) ->
        Reply = ok,
        Return = {reply, Reply, State},
        io:format("handle_call: ~p~n", [Return]),
        Return.
    
    handle_cast(_Msg, State) ->
        Return = {noreply, State},
        io:format("handle_cast: ~p~n", [Return]),
        Return.
    
    handle_info(_Info, State) ->
        Return = {noreply, State},
        io:format("handle_info: ~p~n", [Return]),
        Return.
    
    terminate(_Reason, _State) ->
        Return = ok,
        io:format("terminate: ~p~n", [Return]),
        ok.
    
    code_change(_OldVsn, State, _Extra) ->
        Return = {ok, State},
        io:format("code_change: ~p~n", [Return]),
        Return.

This code is simple: every message received is printed to standard output.

## gen_server behaviour

To define a `gen_server`, you need to explicitly declare it in your source code with `-behaviour(gen_server)`. Note, `behaviour` can be written in US (behavior) or UK (behaviour).

## start_link/0

This function is a simple shortcut to call another function: `gen_server:start_link/3,4`.

## start_link/3,4

    start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

This function is called when you want to start your server linked to a `supervisor` or another process. `start_link/3,4` can register automatically your process (if you think your process need to be unique) or can simply spawn it like simple process. When called, this function execute `init/1`.

This function can return these define values:

 * `{ok,Pid}`
 * `ignore`
 * `{error,Error}`

## [init/1][1]

    init([]) ->
        State = [],
        {ok, State}.

`init/1` is the first executed function when your server will be launched. This one initialize all prerequisite of your application and return state to newly created process.

This function can return only these defined values:

 * `{ok,State}`
 * `{ok,State,Timeout}`
 * `{ok,State,hibernate}`
 * `{stop,Reason}`
 * `ignore`

`State` variable can be everything, (e.g. list, tuple, proplists, map, record) and remain accessible to all function inside spawned process.

## [handle_call/3][2]

    handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

`gen_server:call/2` execute this callback. The first argument is your message (`_Request`), the second is the origin of the request (`_From`) and the last one is the current state (`State`) of your running gen_server behaviour.

If you want a reply to caller, `handle_call/3` need to return one of these data structure:

 * `{reply,Reply,NewState}`
 * `{reply,Reply,NewState,Timeout}`
 * `{reply,Reply,NewState,hibernate}`

If you want no reply to caller, `handle_call/3` need to return one of these data structure:

 * `{noreply,NewState}`
 * `{noreply,NewState,Timeout}`
 * `{noreply,NewState,hibernate}`

If you want to stop the current execution of your current gen_server, `handle_call/3` need to return one of these data structure:

 * `{stop,Reason,Reply,NewState}`
 * `{stop,Reason,NewState}`

## [handle_cast/2][3]

    handle_cast(_Msg, State) ->
        {noreply, State}.

`gen_server:cast/2` execute this callback. The first argument is your message (`_Msg`), and the second the current state of your running gen_server behaviour.

By default, this function can't data to the caller, so, you have only two choices, continue current execution:

 * `{noreply,NewState}`
 * `{noreply,NewState,Timeout}`
 * `{noreply,NewState,hibernate}`

Or stop your current `gen_server` process:

 * `{stop,Reason,NewState}`

## [handle_info/2][4]

    handle_info(_Info, State) ->
        {noreply, State}.

`handle_info/2` is executed when non-standard OTP message come from outside world. This one can't reply and, like `handle_cast/2` can do only 2 actions, continuing current execution:

 * `{noreply,NewState}`
 * `{noreply,NewState,Timeout}`
 * `{noreply,NewState,hibernate}`

Or stop the current running `gen_server` process:

 * `{stop,Reason,NewState}`

## [terminate/2][5]

    terminate(_Reason, _State) ->
        ok.

`terminate/2` is called when an error occur or when you want to shutdown your `gen_server` process.

## [code_change/3][6]

    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

`code_change/3` function is called when you want to upgrade your running code.

This function can return only these defined values:

 * `{ok, NewState}`
 * `{error, Reason}`

# Starting This process

You can compile your code and start `simple_gen_server`:

    simple_gen_server:start_link().

If you want to send message to your server, you can use these functions:

    % will use handle_call as callback and print:
    %   handle_call: mymessage
    gen_server:call(simple_gen_server, mymessage).

    % will use handle_cast as callback and print:
    %   handle_cast: mymessage
    gen_server:cast(simple_gen_server, mymessage).

    % will use handle_info as callback and print:
    %   handle_info: mymessage
    erlang:send(whereis(simple_gen_server), mymessage).


  [1]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L116
  [2]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L119
  [3]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L127
  [4]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L131
  [5]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L135
  [6]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L139
  [7]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L199
  [8]: https://github.com/erlang/otp/blob/OTP-19.0/lib/stdlib/src/gen_server.erl#L218

## Simple Key/Value Database
This source code create a simple [key/value store][1] service based on [`map`][2] Erlang datastructure. Firstly, we need to define all information concerning our `gen_server`:

    -module(cache).
    -behaviour(gen_server).
    
    % our API
    -export([start_link/0]).
    -export([get/1, put/2, state/0, delete/1, stop/0]).
    
    % our handlers
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
             terminate/2, code_change/3]).

    % Defining our function to start `cache` process:

    start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

    %%%%%%%%%%%%%%
    % API

    % Key/Value database is a simple store, value indexed by an unique key. 
    % This implementation is based on map, this datastructure is like hash 
    # in Perl or dictionaries in Python. 

    % put/2
    % put a value indexed by a key. We assume the link is stable 
    % and the data will be written, so, we use an asynchronous call with 
    % gen_server:cast/2.

    put(Key, Value) ->
        gen_server:cast(?MODULE, {put, {Key, Value}}).

    % get/1
    % take one argument, a key and will a return the value indexed 
    % by this same key. We use a synchronous call with gen_server:call/2.

    get(Key) ->
        gen_server:call(?MODULE, {get, Key}).

    % delete/1 
    % like `put/1`, we assume the data will be removed. So, we use an 
    % asynchronous call with gen_server:cast/2.

    delete(Key) ->
        gen_server:cast(?MODULE, {delete, Key}).

    % state/0 
    % This function will return the current state (here the map who contain all 
    % indexed values), we need a synchronous call.

    state() ->
        gen_server:call(?MODULE, {get_state}).

    % stop/0
    % This function stop cache server process.

    stop() ->
        gen_server:stop(?MODULE).

    %%%%%%%%%%%%%%%
    % Handlers

    % init/1
    % Here init/1 will initialize state with simple empty map datastructure.

    init([]) ->
        {ok, #{}}.

    % handle_call/3
    % Now, we need to define our handle. In a cache server we need to get our 
    % value from a key, this feature need to be synchronous, so, using 
    % handle_call seems a good choice:

    handle_call({get, Key}, From, State) ->
        Response = maps:get(Key, State, undefined),
        {reply, Response, State};

    % We need to check our current state, like get_fea

    handle_call({get_state}, From, State) ->
        Response = {current_state, State},
        {reply, Response, State};

    % All other messages will be dropped here.

    handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

    % handle_cast/2
    % put/2 will execute this function.

    handle_cast({put, {Key, Value}}, State) ->
        NewState = maps:put(Key, Value, State),
        {noreply, NewState};

    % delete/1 will execute this function.

    handle_cast({delete, Key}, State) ->
        NewState = maps:remove(Key, State),
        {noreply, NewState};

    % All other messages are dropped here.

    handle_cast(_Msg, State) ->
        {noreply, State}.

    %%%%%%%%%%%%%%%%
    % other handlers

    % We don't need other features, other handlers do nothing.

    handle_info(_Info, State) ->
        {noreply, State}.
    
    terminate(_Reason, _State) ->
        ok.
    
    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

## Using our cache server

We can now compile our code and start using it with `erl`.

    % compile cache
    c(cache).

    % starting cache server
    cache:start_link().

    % get current store
    % will return:
    %   #{}
    cache:state().
  
    % put some data
    cache:put(1, one).
    cache:put(hello, bonjour).
    cache:put(list, []).

    % get current store
    % will return:
    %   #{1 => one,hello => bonjour,list => []}
    cache:state().
    
    % delete a value
    cache:delete(1).
    cache:state().
    %   #{1 => one,hello => bonjour,list => []}

    % stopping cache server
    cache:stop().


[1]: https://en.wikipedia.org/wiki/Key-value_database
[2]: http://erlang.org/doc/man/maps.html

