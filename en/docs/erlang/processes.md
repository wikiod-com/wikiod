---
title: "Processes"
slug: "processes"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Creating Processes
We create a new concurrent process by calling the `spawn` function. The `spawn` function will get as parameter a function `Fun` that the process will evaluate. The return value of the `spawn` function is the created process identifier (pid).

    1> Fun = fun() -> 2+2 end.
    #Fun<erl_eval.20.52032458>
    2> Pid = spawn(Fun).
    <0.60.0>

You can also use `spawn/3` to start a process that will execute a specific function from a module: `spawn(Module, Function, Args)`.  
Or use `spawn/2` or `spawn/4` similarly to start a process in a different node: `spawn(Node, Fun)` or `spawn(Node, Module, Function, Args)`.
 

## Message Passing
Two erlang processes can communicate with each other, wich is also known as _message passing_.  
This procedure is _asynchronous_ in the form that the sending process will not halt after sending the message. 
## Sending Messages ##
This can be achieved with the construct ```Pid ! Message```, where ```Pid``` is a valid process identifier (pid) and ```Message``` is a value of any data type.  

Each process has a "mailbox" that contains the received messages in the received order. This "mailbox" can be emptied with the build in ```flush/0``` function.

If a message is send to a non existing process, the message will be discarded without any error! 

An example might look like the following, where ```self/0``` returns the pid of the current process and ```pid/3``` creates a pid.

    1> Pidsh = self().
    <0.32.0>
    2> Pidsh ! hello.
    hello
    3> flush().
    Shell got hello
    ok
    4> <0.32.0> ! hello.
    * 1: syntax error before: ’<’
    5> Pidsh2 = pid(0,32,0).
    <0.32.0>
    6> Pidsh2 ! hello2.
    hello2
    7> flush().
    Shell got hello2
    ok

It is also possible to send a message to multiple processes at once, with ```Pid3!Pid2!Pid1!Msg```.

## Receiving Messages ##

Received messages can be processed with the ```receive``` construct.  

    receive
      Pattern1            -> exp11, .., exp1n1;
      Pattern2 when Guard -> exp21, .., exp2n2;
      ...
      Other               -> exp31, .., exp3n3;
      ...
      after Timeout       -> exp41, .., exp4n4
    end

The ```Pattern``` will be compared to the messages in the "mailbox" starting with the first and oldest message.  
If a pattern matches, the matched message is removed from the "mailbox" and the clause body is evaluated.  

It is also possible to define timeouts with the ```after``` construct.  
A ```Timeout``` is either the waiting time in milliseconds or the atom ```infinity```.

The return value of ```receive``` is the last evaluated clause body.  

## Example (Counter) ##

A (very) simple counter with message passing might look like in the following.
    
    -module(counter0).
    -export([start/0,loop/1]).
    
    % Creating the counter process.
    start() ->
        spawn(counter0, loop, [0]).

    % The counter loop.
    loop(Val) ->
        receive
            increment ->
               loop(Val + 1)
        end.

Interaction with the counter.

    1> C0 = counter0:start().
    <0.39.0>
    2> C0!increment.
    increment
    3> C0!increment.
    increment

## Register Processes
It is possible to register a process (pid) to a global alias.  
This can be achieved with the build in ```register(Alias, Pid)``` function, where ```Alias``` is the atom to access the process as and ```Pid``` is the process id.  

The alias will be globally available!  
It is very easy to create shared state, wich is usually not preferable. ([See also here][1])  

It is possible to unregister a process with ```unregister(Pid)``` and receive the pid from an alias with ```whereis(Alias)```.

Use ```registered()``` for a list of all registered aliases.

The example registers the Atom foo to the pid of the current process and sends a message using the registered Atom.

    1> register(foo, self()).
    true
    2> foo ! 'hello world'.
    'hello world'




  [1]: https://stackoverflow.com/questions/2312577/in-erlang-how-is-using-registered-processes-different-from-global-variables-in-a#2319050

