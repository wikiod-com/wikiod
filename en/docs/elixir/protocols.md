---
title: "Protocols"
slug: "protocols"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

**A note on structs**

Instead of sharing protocol implementation with maps, structs require their own protocol implementation.

## Introduction
Protocols enable polymorphism in Elixir. Define protocols with `defprotocol`:

    defprotocol Log do
      def log(value, opts)
    end
Implement a protocol with `defimpl`:
    
    require Logger
    # User and Post are custom structs
    
    defimpl Log, for: User do
      def log(user, _opts) do
        Logger.info "User: #{user.name}, #{user.age}"
      end
    end

    defimpl Log, for: Post do
      def log(user, _opts) do
        Logger.info "Post: #{post.title}, #{post.category}"
      end
    end

With the above implementations, we can do:

    iex> Log.log(%User{name: "Yos", age: 23})
    22:53:11.604 [info]  User: Yos, 23
    iex> Log.log(%Post{title: "Protocols", category: "Protocols"})
    22:53:43.604 [info]  Post: Protocols, Protocols

Protocols let you dispatch to any data type, so long as it implements the protocol. This includes some built-in types such as `Atom`, `BitString`, `Tuples`, and others.



