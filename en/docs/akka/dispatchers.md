---
title: "Dispatchers"
slug: "dispatchers"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Default Dispatcher
An Akka MessageDispatcher is what makes Akka Actors "tick", it is the engine of the machine so to speak. All MessageDispatcher implementations are also an ExecutionContext, which means that they can be used to execute arbitrary code, for instance Futures.

Every ActorSystem will have a default dispatcher that will be used in case nothing else is configured for an Actor. The default dispatcher can be configured, and is by default a `Dispatcher` with the specified `default-executor`. If an ActorSystem is created with an `ExecutionContext` passed in, this ExecutionContext will be used as the default executor for all dispatchers in this ActorSystem. If no ExecutionContext is given, it will fallback to the executor specified in `akka.actor.default-dispatcher.default-executor.fallback`. By default this is a `fork-join-executor`, which gives excellent performance in most cases.

## Setting the dispatcher for an Actor
So in case you want to give your Actor a different dispatcher than the default, you need to do two things, of which the first is to configure the dispatcher in your `application.conf`:

    my-dispatcher {
      # Dispatcher is the name of the event-based dispatcher
      type = Dispatcher
      # What kind of ExecutionService to use
      executor = "fork-join-executor"
      # Configuration for the fork join pool
      fork-join-executor {
        # Min number of threads to cap factor-based parallelism number to
        parallelism-min = 2
        # Parallelism (threads) ... ceil(available processors * factor)
        parallelism-factor = 2.0
        # Max number of threads to cap factor-based parallelism number to
        parallelism-max = 10
      }
      # Throughput defines the maximum number of messages to be
      # processed per actor before the thread jumps to the next actor.
      # Set to 1 for as fair as possible.
      throughput = 100
    }

And here's another example that uses the "thread-pool-executor":

    my-thread-pool-dispatcher {
      # Dispatcher is the name of the event-based dispatcher
      type = Dispatcher
      # What kind of ExecutionService to use
      executor = "thread-pool-executor"
      # Configuration for the thread pool
      thread-pool-executor {
        # minimum number of threads to cap factor-based core number to
        core-pool-size-min = 2
        # No of core threads ... ceil(available processors * factor)
        core-pool-size-factor = 2.0
        # maximum number of threads to cap factor-based number to
        core-pool-size-max = 10
      }
      # Throughput defines the maximum number of messages to be
      # processed per actor before the thread jumps to the next actor.
      # Set to 1 for as fair as possible.
      throughput = 100
    }

You can then define the dispatcher to use for your actor inside you config, e.g.

    akka.actor.deployment {
      /myactor {
        dispatcher = my-dispatcher
      }
    }

and create this actor with the name specified in the config:

    import akka.actor.Props
    val myActor = context.actorOf(Props[MyActor], "myactor")

Or you can lookup your dispatcher with:

    import akka.actor.Props
    val myActor =
      context.actorOf(Props[MyActor].withDispatcher("my-dispatcher"), "myactor1")

