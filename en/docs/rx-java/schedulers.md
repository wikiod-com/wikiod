---
title: "Schedulers"
slug: "schedulers"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Basic Examples
Schedulers are an RxJava abstraction about processing unit. A scheduler can be backed by a Executor service, but you can implement your own scheduler implementation.

A `Scheduler` should meet this requirement : 

- Should process undelayed task sequencially (FIFO order)
- Task can be delayed

A `Scheduler` can be used as parameter in some operators (example : `delay`), or used with the `subscribeOn` / `observeOn` method.

With some operator, the `Scheduler` will be used to process the task of the specific operator. For example, `delay` will schedule a delayed task that will emit the next value. This is a `Scheduler` that will retain and execute it later.

The `subscribeOn` can be used once per `Observable`. It will define in which `Scheduler` the code of the subscription will be executer.

The `observeOn` can be used multiple times per `Observable`. It will define in which `Scheduler` will be used to execute all tasks defined __after__ the `observeOn` method. `observeOn` will help you to perform thread hopping. 

**subscribeOn specific Scheduler**

    // this lambda will be executed in the `Schedulers.io()`
    Observable.fromCallable(() -> Thread.currentThread().getName())
              .subscribeOn(Schedulers.io())
              .subscribe(System.out::println); 

**observeOn with specific Scheduler**

    Observable.fromCallable(() -> "Thread -> " + Thread.currentThread().getName())
             // next tasks will be executed in the io scheduler
             .observeOn(Schedulers.io())
             .map(str -> str + " -> " + Thread.currentThread().getName())
              // next tasks will be executed in the computation scheduler
             .observeOn(Schedulers.computation())
             .map(str -> str + " -> " + Thread.currentThread().getName())
             // next tasks will be executed in the io scheduler
             .observeOn(Schedulers.newThread())
             .subscribe(str -> System.out.println(str + " -> " + Thread.currentThread().getName()));   
             
**Specifying a specific Scheduler with an operator**

Some operators can take a `Scheduler` as parameter.

    Observable.just(1)
              // the onNext method of the delay operator will be executed in a new thread
              .delay(1, TimeUnit.SECONDS, Schedulers.newThread())
              .subscribe(System.out::println);
  
**Publish To Subscriber:**

    TestScheduler testScheduler = Schedulers.test();
    EventBus sut = new DefaultEventBus(testScheduler);
    TestSubscriber<Event> subscriber = new TestSubscriber<Event>();
    sut.get().subscribe(subscriber);
    sut.publish(event);
    testScheduler.advanceTimeBy(1, TimeUnit.SECONDS);

**Thread Pool:**

    this.poolName = schedulerFig.getIoSchedulerName();
    final int poolSize = schedulerFig.getMaxIoThreads();
    final BlockingQueue<Runnable> queue = new ArrayBlockingQueue<Runnable>(poolSize);
    final MaxSizeThreadPool threadPool = new MaxSizeThreadPool( queue, poolSize );
    this.scheduler = Schedulers.from(threadPool);

**Web Socket Observable:**

    final Subscription subscribe = socket.webSocketObservable()
            .subscribeOn(Schedulers.io())
            .doOnNext(new Action1<RxEvent>() {
                @Override
                public void call(RxEvent rxEvent) {
                    System.out.println("Event: " + rxEvent);
                }
            })
            .subscribe();

