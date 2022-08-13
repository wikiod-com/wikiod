---
title: "Processes"
slug: "processes"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Spawning a Simple Process
In the following example, the `greet` function inside `Greeter` module is run in a separate process:
``` elixir
defmodule Greeter do
    def greet do
        IO.puts "Hello programmer!"
    end
end

iex> spawn(Greeter, :greet, []) 
Hello
#PID<0.122.0>
```

Here `#PID<0.122.0>` is the *process identifier* for the spawned process.

## Sending and Receiving Messages
```
defmodule Processes do
    def receiver do
        receive do
            {:ok, val} ->
                IO.puts "Received Value: #{val}"
            _ ->
                IO.puts "Received something else"
        end
    end
end
```

```
iex(1)> pid = spawn(Processes, :receiver, [])
#PID<0.84.0>
iex(2)> send pid, {:ok, 10}
Received Value: 10
{:ok, 10}
```

## Recursion and Receive
Recursion can be used to receive multiple messages
```
defmodule Processes do
    def receiver do
        receive do
            {:ok, val} ->
                IO.puts "Received Value: #{val}"
            _ ->
                IO.puts "Received something else"
        end
        receiver
    end
end
```

```
iex(1)> pid = spawn Processes, :receiver, []
#PID<0.95.0>
iex(2)> send pid, {:ok, 10}
Received Value: 10
{:ok, 10}
iex(3)> send pid, {:ok, 42}
{:ok, 42}
Received Value: 42
iex(4)> send pid, :random
:random
Received something else
```
Elixir will use a tail-call recursion optimisation as long as the function call is the last thing that happens in the function as it is in the example.

