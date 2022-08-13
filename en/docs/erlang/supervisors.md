---
title: "Supervisors"
slug: "supervisors"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Basic supervisor with one worker process
This example uses the map format introduced in Erlang/OTP 18.0.

    %% A module implementing a supervisor usually has a name ending with `_sup`.
    -module(my_sup).
    
    -behaviour(supervisor).

    %% API exports
    -export([start_link/0]).

    %% Behaviour exports
    -export([init/1]).

    start_link() ->
        %% If needed, we can pass an argument to the init callback function.
        Args = [],
        supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

    %% The init callback function is called by the 'supervisor' module.
    init(_Args) ->
        %% Configuration options common to all children.
        %% If a child process crashes, restart only that one (one_for_one).
        %% If there is more than 1 crash ('intensity') in
        %% 5 seconds ('period'), the entire supervisor crashes
        %% with all its children.
        SupFlags = #{strategy => one_for_one,
                     intensity => 1,
                     period => 5},

        %% Specify a child process, including a start function.
        %% Normally the module my_worker would be a gen_server
        %% or a gen_fsm.
        Child = #{id => my_worker,
                  start => {my_worker, start_link, []}},

        %% In this case, there is only one child.
        Children = [Child],

        %% Return the supervisor flags and the child specifications
        %% to the 'supervisor' module.
        {ok, {SupFlags, Children}}.

