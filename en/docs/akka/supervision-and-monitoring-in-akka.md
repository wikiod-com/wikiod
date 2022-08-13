---
title: "Supervision and Monitoring in Akka"
slug: "supervision-and-monitoring-in-akka"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

**References:** [akka.io/docs][1]

**Check out my blog:** https://blog.knoldus.com/2016/08/07/supervision-and-monitoring-in-akka/

  [1]: http://akka.io/docs/

## What is supervision?
Describes a dependency relationship between actors, the parent and child releationship. Parent is unique because it has created the child actor, so the parent is responsible for reacting when failures happens in his child.

And parent decides which choice needs to be selected. When a parent receives the failure signal from it’s child then depending on the nature of failure, the parent decides from following options:

Resume: Parent starts the child actor keeping its internal state.

Restart: Parent starts the child actor by clearing it’s internal state.

Stop: Stop the child permanently.

Escalate: Escalate the failure by failing itself and propagate failure to its parent.

[![Akka Life Cycle][1]][1]

> Akka Life Cycle


It is always important to view a part of supervision hierarchy, which explains the escalate option. Each supervisor should cover with all possible failure cases.

[![Actor System][2]][2]

> Actor System: Source: doc.akka.io

/user: **The User Guardian Actor**

Actor created using system.actorOf() are children of user guardian actor. Whenever user guardian terminates, all user created actors will be terminated too. Top level user created actors are determined by user guardian actor that how they will be supervised. Root Guardian is the supervisor of user guardian.

/root: **The Root Guardian**

The root guardian actor is the father of all actor system. It supervises user guardian actor and system guardian actor.


  [1]: https://i.stack.imgur.com/pwsm7.png
  [2]: https://i.stack.imgur.com/LVoEe.png

## Supervision Strategies
There are two type of supervision strategies that we follow to supervise any actor:

 1. One-For-One Strategy
    
 2. One-For-All Strategy

 <pre>
case object ResumeException extends Exception
 
case object StopException extends Exception
case object RestartException extends Exception
 
override val supervisorStrategy =
 OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 second){
 case ResumeException => Resume
 case RestartException => Restart
 case StopException => Stop
 case _: Exception => Escalate
 }</pre>





## What is Monitoring?
> Lifecycle Monitoring in Akka is usually referred to as DeathWatch.

Monitoring is thus used to tie one actor to another so that it may react to the other actor’s termination, in contrast to supervision which reacts to failure.

[![Monitoring][1]][1]

> Monitoring

Monitoring is particularly useful if a supervisor cannot simply restart its children and has to terminate them, e.g. in case of errors during actor initialization. In that case it should monitor those children and re-create them or schedule itself to retry this at a later time.


  [1]: https://i.stack.imgur.com/j1z5b.png

## Code Repository
[Supervision and Monitoring][1]


  [1]: https://github.com/knoldus/supervision-monitoring-in-akka

