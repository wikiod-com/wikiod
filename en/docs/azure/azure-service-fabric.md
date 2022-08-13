---
title: "Azure Service Fabric"
slug: "azure-service-fabric"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Azure Service Fabric is one of the PaaS services offered by Azure.
It is based on the notion of containers and services: unlike Compute services (Web Roles and Worker roles), your code does not run inside one (or more) Virtual Machines, but instead they are run inside a container, sharing it with other services.

It is important to note that here, and throughout Microsoft articles and documentation on Service Fabric, *container* is intended in its more general meaning, not as a "Docker" style container.

You can have (and you'll typically have) more than one container, which will form a cluster.
Services running inside the container can be, in increasing order of "awareness"

- Guest executables
- "Reliable" services
    - Stateful 
    - Stateless
- "Reliable" actors



## Reliable actors
An actor inside Service Fabric is defined by a standard .NET interface/class pair:

    public interface IMyActor : IActor
    {
        Task<string> HelloWorld();
    }


    internal class MyActor : Actor, IMyActor
    {
        public Task<string> HelloWorld()
        {
            return Task.FromResult("Hello world!");
        }
    }

Every method in the interface/class pair must be async, and they cannot have out or ref paramenters. 

It is easy to understand why if you think about the actor model: objects interacting with each other through exchange of messages. 
The messages are delivered to an actor class through the async methods; responses are handled by the actors runtime (the actor "container") and routed back to the caller. 

The Service Fabric SDK will generate a proxy at compile time. This proxy is used by the actor client to call its methods (i.e. to deliver a message to the actor and `await` a response).

The client identifies an Actor through a ID. The ID can be already known (you got it from a DB, from another Actor, or maybe it is the userID linked to that actor, or again the serial number of a real object).

If you need to create a new actor and you just need an ID, the (provided) ActorId class has methods to create a randomly distributed actor ID

    ActorId actorId = ActorId.NewId();
    
Then, you can use the `ActorProxy` class to creates a proxy object for the actor. This does not activate an actor or invoke any methods yet.
    IMyActor myActor = ActorProxy.Create<IMyActor>(actorId, new Uri("fabric:/MyApp/MyActorService"));
    
Then, you can use the proxy to invoke a method on the actor. If an actor with the given ID does not exist, it will be activated (created inside one of the containers in the cluster), and then the runtime will post a message to the actor, executing its method call and completing the Task when the actor answers:

    await myActor.HelloWorld();




