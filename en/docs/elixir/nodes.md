---
title: "Nodes"
slug: "nodes"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Connecting nodes on the same machine
Start two named nodes in two terminal windows:

    >iex --name bob@127.0.0.1
    iex(bob@127.0.0.1)>
    >iex --name frank@127.0.0.1
    iex(frank@127.0.0.1)>

Connect two nodes by instructing one node to connect:

    iex(bob@127.0.0.1)> Node.connect :"frank@127.0.0.1"
    true

The two nodes are now connected and aware of each other:

    iex(bob@127.0.0.1)> Node.list
    [:"frank@127.0.0.1"]
    iex(frank@127.0.0.1)> Node.list
    [:"bob@127.0.0.1"]

You can execute code on other nodes:

    iex(bob@127.0.0.1)> greet = fn() -> IO.puts("Hello from #{inspect(Node.self)}") end
    iex(bob@127.0.0.1)> Node.spawn(:"frank@127.0.0.1", greet)
    #PID<9007.74.0>
    Hello from :"frank@127.0.0.1"
    :ok

## Connecting nodes on different machines
Start a named process on one IP address:

    $ iex --name foo@10.238.82.82 --cookie chocolate
    iex(foo@10.238.82.82)> Node.ping :"bar@10.238.82.85"
    :pong
    iex(foo@10.238.82.82)> Node.list
    [:"bar@10.238.82.85"]

Start another named process on a different IP address:

    $ iex --name bar@10.238.82.85 --cookie chocolate
    iex(bar@10.238.82.85)> Node.list
    [:"foo@10.238.82.82"]



## List all visible nodes in the system
    iex(bob@127.0.0.1)> Node.list
    [:"frank@127.0.0.1"]

