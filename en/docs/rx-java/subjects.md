---
title: "Subjects"
slug: "subjects"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Syntax
 - Subject<T, R> subject = AsyncSubject.create(); // Default AsyncSubject
 - Subject<T, R> subject = BehaviorSubject.create(); // Default BehaviorSubject
 - Subject<T, R> subject = PublishSubject.create(); // Default PublishSubject
 - Subject<T, R> subject = ReplaySubject.create(); // Default ReplaySubject
 - mySafeSubject = new SerializedSubject(unSafeSubject);
 // Convert an unsafeSubject to a safeSubject - generally for multi threaded Subjects



## Parameters
| Parameters | Details |
|------------|---------|
|T           |Input type|
|R           |Output type|

This documentation provides details and explanations about `Subject`. For more information and further reading, please visit the [official documentation][1].


  [1]: http://reactivex.io/documentation/subject.html

## PublishSubject
`PublishSubject` emits to an `Observer` only those items that are emitted by the source `Observable` subsequent to the time of the subscription. 

A simple `PublishSubject` example:
   
    Observable<Long> clock = Observable.interval(500, TimeUnit.MILLISECONDS);
    Subject<Long, Long> subjectLong = PublishSubject.create();

    clock.subscribe(subjectLong);

    System.out.println("sub1 subscribing...");
    subjectLong.subscribe(l -> System.out.println("sub1 -> " + l));
    Thread.sleep(3000);
    System.out.println("sub2 subscribing...");
    subjectLong.subscribe(l -> System.out.println("sub2 -> " + l));
    Thread.sleep(5000);

Output:

    sub1 subscribing...
    sub1 -> 0
    sub1 -> 1
    sub2 subscribing...
    sub1 -> 2
    sub2 -> 2
    sub1 -> 3
    sub2 -> 3

In the above example, a `PublishSubject` subscribes to an `Observable` which acts like a clock, and emits items(Long) every 500 milli seconds. As seen in the output, the `PublishSubject` passes on the vales it gets from the source (`clock`) to its subscribers(`sub1` and `sub2`).

A `PublishSubject` can start emitting items as soon as it is created, without any observer, which runs the risk of one or more items being lost till a observer can sunscribe. 

    createClock(); // 3 lines moved for brevity. same as above example

    Thread.sleep(5000); // introduces a delay before first subscribe

    sub1andsub2(); // 6 lines moved for brevity. same as above example

Output: 

    sub1 subscribing...
    sub1 -> 10
    sub1 -> 11
    sub2 subscribing...
    sub1 -> 12
    sub2 -> 12
    sub1 -> 13
    sub2 -> 13

Notice that `sub1` emits values starting from `10`. The 5 second delay introduced caused a *loss* of items. These cannot be reproduces. This essentially makes `PublishSubject` a `Hot Observable`. 

Also, note that if an observer subscribes to the `PublishSubject` after it has emitted ***n*** items, these ***n*** items *cannot* be reproduced for this observer.

Below is the marble diagram of `PublishSubject`

[![enter image description here][1]][1]

The `PublishSubject` emits items to all that have subscribed, at any point of time before the `onCompleted` of the source `Observable` is called.

If the source `Observable` terminates with an error, the `PublishSubject` will not emit any items to subsequent observers, but will simply pass along the error notification from the source Observable.

[![enter image description here][2]][2]

**Use Case**<br>
Suppose you want to create an application that will monitor the stock prices of a certain company and forward it to all clients who request for it. 

    /* Dummy stock prices */
    Observable<Integer> prices = Observable.just(11, 12, 14, 11, 10, 12, 15, 11, 10);

    /* Your server */
    PublishSubject<Integer> watcher = PublishSubject.create();
    /* subscribe to listen to stock price changes and push to observers/clients */
    prices.subscribe(watcher);
    
    /* Client application */
    stockWatcher = getWatcherInstance(); // gets subject
    Subscription steve = stockWatcher.subscribe(i -> System.out.println("steve watching " + i));
    Thread.sleep(1000);
    System.out.println("steve stops watching");
    steve.unsubscribe();

In the above example use case, the `PublishSubject` acts as a bridge to pass on the values from your server to all the clients that subscribe to your `watcher`.

Further reading:

 - PublishSubject [javadocs][3]
 - [Blog][4] by Thomas Nield (Advanced reading)

 


  [1]: http://i.stack.imgur.com/UKFxw.jpg
  [2]: http://i.stack.imgur.com/BlLyD.jpg
  [3]: http://reactivex.io/RxJava/javadoc/rx/subjects/PublishSubject.html
  [4]: http://tomstechnicalblog.blogspot.in/2016/03/rxjava-problem-with-subjects.html

## Basic Subjects
A `Subject` in RxJava is a class that is both an `Observable` and an `Observer`. This basically means that it can act as an `Observable` and pass inputs to subscribers and as an `Observer` to get inputs from another Observable.


    Subject<String, String> subject = PublishSubject.create(); 
    subject.subscribe(System.out::print);
    subject.onNext("Hello, World!"); 

The above prints "Hello, World!" to console using `Subjects`.

**Explanation**

 1. The first line of code defines a new `Subject` of type `PublishSubject`

        Subject<String, String> subject = PublishSubject.create();
            |     |       |       |                 |
         subject<input, output>  name   = default publish subject

 2. The second line subscribes to the subject, showing the `Observer` behaviour.

        subject.subscribe(System.out::print);
    This enables the `Subject` to take inputs like a regular subscriber

 3. The third line calls the `onNext` method of the subject, showing the `Observable` behaviour.

        subject.onNext("Hello, World!"); 

    This enables the `Subject` to give inputs to all subscribing to it.

**Types**

A `Subject` (in RxJava) can be of any of these four types:

 - AsyncSubject
 - BehaviorSubject
 - PublishSubject
 - ReplaySubject

Also, a `Subject` can be of type `SerializedSubject`. This type ensures that the `Subject` does not violate to the *Observable Contract*  (which specifies that all calls must be Serialized)

Further reading:

 - [To Use or Not to Use Subject][1] from Dave Sexton’s blog


  [1]: http://davesexton.com/blog/post/To-Use-Subject-Or-Not-To-Use-Subject.aspx

