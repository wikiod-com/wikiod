---
title: "Actor DSL"
slug: "actor-dsl"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Simple Actor DSL
To create simple actors without creating a new class, you can use:

    import akka.actor.ActorDSL._
    import akka.actor.ActorSystem
     
    implicit val system = ActorSystem("demo")

    val a = actor(new Act {
      become {
        case "hello" ⇒ sender() ! "hi"
      }
    })

## Context switching
The two possible ways of issuing a context.become (replacing or adding the new behavior) are offered separately to enable a clutter-free notation of nested receives:

    val a = actor(new Act {
      become { // this will replace the initial (empty) behavior
        case "info" ⇒ sender() ! "A"
        case "switch" ⇒
          becomeStacked { // this will stack upon the "A" behavior
            case "info"   ⇒ sender() ! "B"
            case "switch" ⇒ unbecome() // return to the "A" behavior
          }
        case "lobotomize" ⇒ unbecome() // OH NOES: Actor.emptyBehavior
      }
    })

## Life-cycle Management
Life-cycle hooks are also exposed as DSL elements, where later invocations of the methods shown below will replace the contents of the respective hooks:

    val a = actor(new Act {
      whenStarting { testActor ! "started" }
      whenStopping { testActor ! "stopped" }
    })

The above is enough if the logical life-cycle of the actor matches the restart cycles (i.e. whenStopping is executed before a restart and whenStarting afterwards). If that is not desired, use the following two hooks:

    val a = actor(new Act {
      become {
        case "die" ⇒ throw new Exception
      }
      whenFailing { case m @ (cause, msg) ⇒ testActor ! m }
      whenRestarted { cause ⇒ testActor ! cause }
    })



## Nested Actors
It is also possible to create nested actors, i.e. grand-children, like this:

    // here we pass in the ActorRefFactory explicitly as an example
    val a = actor(system, "fred")(new Act {
      val b = actor("barney")(new Act {
        whenStarting { context.parent ! ("hello from " + self.path) }
      })
      become {
        case x ⇒ testActor ! x
      }
    })

## Supervision
It is also possible to assign a supervision strategy to these actors with the following:

    superviseWith(OneForOneStrategy() {
      case e: Exception if e.getMessage == "hello" ⇒ Stop
      case _: Exception                            ⇒ Resume
    })

## Stash support
Last but not least there is a little bit of convenience magic built-in, which detects if the runtime class of the statically given actor subtype extends the RequiresMessageQueue trait via the Stash trait (this is a complicated way of saying that new Act with Stash would not work because its runtime erased type is just an anonymous subtype of Act). The purpose is to automatically use the appropriate deque-based mailbox type required by Stash. If you want to use this magic, simply extend ActWithStash:

    val a = actor(new ActWithStash {
      become {
        case 1 ⇒ stash()
        case 2 ⇒
          testActor ! 2; unstashAll(); becomeStacked {
            case 1 ⇒ testActor ! 1; unbecome()
          }
      }
    })



