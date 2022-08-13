---
title: "Observable"
slug: "observable"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Create an Observable
There are several ways to create an Observable in RxJava. The most powerful way is to use the `Observable.create` method. But it's also the most __complicated way__. So you must __avoid using it__, as much as possible.

# Emitting an exiting value

If you already have a value, you can use `Observable.just` to emit your value. 
    
     Observable.just("Hello World").subscribe(System.out::println);

# Emitting a value that should be computed

If you want to emit a value that is not already computed, or that can take long to be computed, you can use `Observable.fromCallable` to emit your next value.
    
    Observable.fromCallable(() -> longComputation()).subscribe(System.out::println);

`longComputation()` will only be called when you subscribe to your `Observable`. This way, the computation will be *lazy*.

# Alternative way to Emitting a value that should be computed

`Observable.defer` builds an `Observable` just like `Observable.fromCallable` but it is used when you need to return an `Observable` instead of a value. It is useful when you want to manage the errors in your call.

    Observable.defer(() -> {
              try {
                    return Observable.just(longComputation());
              } catch(SpecificException e) {
                    return Observable.error(e);
              }).subscribe(System.out::println);

## Hot and Cold Observables
Observables are broadly categorised as `Hot` or `Cold`, depending on their emission behaviour. <br>A `Cold Observable` is one which starts emitting upon request(subscription), whereas a `Hot Observable` is one that emits regardless of subscriptions.

**<h2>Cold Observable</h2>**

    /* Demonstration of a Cold Observable */
    Observable<Long> cold = Observable.interval(500, TimeUnit.MILLISECONDS); // emits a long every 500 milli seconds
    cold.subscribe(l -> System.out.println("sub1, " + l)); // subscriber1
    Thread.sleep(1000); // interval between the two subscribes
    cold.subscribe(l -> System.out.println("sub2, " + l)); // subscriber2

The output of the above code looks like (may vary):

    sub1, 0    -> subscriber1 starts
    sub1, 1
    sub1, 2
    sub2, 0    -> subscriber2 starts
    sub1, 3
    sub2, 1
    sub1, 4
    sub2, 2

Notice that even though `sub2` starts late, it receives values from the start. To conclude, a `Cold Observable` only emits items when requested for. Multiple request start multiple pipelines.

**<h2>Hot Observable</h2>**
*Note: Hot observables emit values independent of individual subscriptions. They have their own timeline and events occur whether someone is listening or not.*<br>

A `Cold Observale` can be converted to a `Hot Observable` with a simple `publish`.

    Observable.interval(500, TimeUnit.MILLISECONDS)
        .publish(); // publish converts cold to hot
`publish` returns a  `ConnectableObservable` that adds functionalities to *connect* and *disconnect* from the observable.

    ConnectableObservable<Long> hot = Observable
                                        .interval(500, TimeUnit.MILLISECONDS)
                                        .publish(); // returns ConnectableObservable
    hot.connect(); // connect to subscribe

    hot.subscribe(l -> System.out.println("sub1, " + l));
    Thread.sleep(1000);
    hot.subscribe(l -> System.out.println("sub2, " + l));
The above yields:

    sub1, 0  -> subscriber1 starts
    sub1, 1
    sub1, 2
    sub2, 2  -> subscriber2 starts
    sub1, 3
    sub2, 3
Notice that even though `sub2` starts observing late, it is in sync with `sub1`.<br>Disconnect is a little more complicated! Disconnect happens on the `Subscription` and not the `Observable`.

    ConnectableObservable<Long> hot = Observable
                                        .interval(500, TimeUnit.MILLISECONDS)
                                        .publish(); // same as above
    Subscription subscription = hot.connect(); // connect returns a subscription object, which we store for further use

    hot.subscribe(l -> System.out.println("sub1, " + l));
    Thread.sleep(1000);
    hot.subscribe(l -> System.out.println("sub2, " + l));
    Thread.sleep(1000);
    subscription.unsubscribe(); // disconnect, or unsubscribe from subscription

    System.out.println("reconnecting");
    /* reconnect and redo */
    subscription = hot.connect();
    hot.subscribe(l -> System.out.println("sub1, " + l));
    Thread.sleep(1000);
    hot.subscribe(l -> System.out.println("sub2, " + l));
    Thread.sleep(1000);
    subscription.unsubscribe();
The above produces:

    sub1, 0   -> subscriber1 starts
    sub1, 1
    sub1, 2
    sub2, 2   -> subscriber2 starts
    sub1, 3
    sub2, 3
    reconnecting  -> reconnect after unsubscribe
    sub1, 0
    ...
Upon disconnect, the `Observable` essentially "terminates" and restarts when a new subscription is added.<br><br>
`Hot Observable` can be used for creating an `EventBus`. Such EventBuses are generally light and super fast. The only downside of an RxBus is that all events must be manually implemented and passed to the bus.

