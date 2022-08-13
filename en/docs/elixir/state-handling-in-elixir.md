---
title: "State Handling in Elixir"
slug: "state-handling-in-elixir"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Managing a piece of state with an Agent
The simplest way to wrap and access a piece of state is `Agent`. The module allows one to spawn a process that keeps an arbitrary data structure and allows one to send messages to read and update that structure. Thanks to this the access to the structure is automatically serialized, as the process only handles one message at a time.

    iex(1)> {:ok, pid} = Agent.start_link(fn -> :initial_value end)
    {:ok, #PID<0.62.0>}
    iex(2)> Agent.get(pid, &(&1))
    :initial_value
    iex(3)> Agent.update(pid, fn(value) -> {value, :more_data} end)
    :ok
    iex(4)> Agent.get(pid, &(&1))
    {:initial_value, :more_data}

