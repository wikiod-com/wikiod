---
title: "Unit Testing"
slug: "unit-testing"
draft: false
images: []
weight: 9842
type: docs
toc: true
---

Because all the Schedulers methods are static, unit tests utilizing the RxJava hooks cannot be ran in parallel on the same JVM instance. If they where, one TestScheduler would be removed in the middle of a unit test. That is basically the downside of using the Schedulers class.


## TestScheduler
<!-- language-all: lang-java -->

TestSchedulers allows you to control time and execution of Observables instead of having to do busy waits, joining threads or anything to manipulate system time. This is VERY important if you want to write unit tests that are predictable, consistent and fast. Because you are manipulating time, there is no longer the chance that a thread got starved, that your test fails on a slower machine or that you waste execution time busy waiting for a result.

TestSchedulers can be provided via the overload that takes a Scheduler for any RxJava operations.

    TestScheduler testScheduler = new TestScheduler();
    TestSubscriber<Integer> subscriber = TestSubscriber.create();
    Observable.just(1,2,3)
              .delay(10, TimeUnit.SECONDS, testScheduler)
              .subscribe(subscriber);

    try {
        Thread.sleep(TimeUnit.SECONDS.toMillis(11));
    } catch (InterruptedException ignored) { }
    subscriber.assertValues(1,2,3); // fails

    testScheduler.advanceTimeBy(10, TimeUnit.SECONDS);
    subscriber.assertValues(1,2,3); // success

The TestScheduler is pretty basic. It only consists of three methods.

    testScheduler.advanceTimeBy(amount, timeUnit);
    testScheduler.advanceTimeTo(when, timeUnit);
    testScheduler.triggerActions();

This lets you manipulate when the TestScheduler should fire all the actions pertaining to some time in the future. 

While passing the scheduler works, this is not how the TestScheduler is commonly used because of how ineffective it is. Passing schedulers into classes ends up providing a lot of extra code for little gain. Instead, you can hook into RxJava's Schedulers.io()/computation()/etc. This is done with RxJava's Hooks. This lets you define what gets returned from a call from one of the Schedulers methods.

    public final class TestSchedulers {
    
        public static TestScheduler test() {
            final TestScheduler testScheduler = new TestScheduler();
            RxJavaHooks.reset();
            RxJavaHooks.setOnComputationScheduler((scheduler) -> {
                return testScheduler;
            });
            RxJavaHooks.setOnIOScheduler((scheduler) -> {
                return testScheduler;
            });
            RxJavaHooks.setOnNewThreadScheduler((scheduler) -> {
                return testScheduler;
            });
            return testScheduler;
        }
    }

This class allows the user to get the test scheduler that will be hooked up for all calls to Schedulers. A unit test would just need to get this scheduler in its setup. It is highly recommend aquiring it in the setup and not as any plain old field because your TestScheduler might try to triggerActions in from another unit test when you advance time. Now our example above becomes
    
    TestScheduler testScheduler = new TestScheduler();
    TestSubscriber<Integer> subscriber = TestSubscriber.create();
    Observable.just(1,2,3)
              .delay(10, TimeUnit.SECONDS, testScheduler)
              .subscribe(subscriber);
    testScheduler.advanceTimeBy(9, TimeUnit.SECONDS);
    subscriber.assertValues(); // success (delay hasn't finished)
    testScheduler.advanceTimeBy(10, TimeUnit.SECONDS);
    subscriber.assertValues(1,2,3); // success (delay has finished)

That's how you can effectively remove the system clock from your unit test (at least as far as RxJava is concerned 😆)





## TestSubscriber
<!-- language-all: lang-java -->

TestSubscribers allow you to avoid the work creating your own Subscriber or subscribe Action<?> to verify that certain values where delivered, how many there are, if the Observable completed, an exception was raised and a whole lot more.

# Getting Started

This example just shows an assertion that the values 1,2,3 and 4 where passed into the Observable via onNext.

    TestSubscriber<Integer> ts = TestSubscriber.create();
    Observable.just(1,2,3,4).subscribe(ts);
    ts.assertValues(1,2,3,4); // Success

`assertValues` asserts that the count is correct. If you were to only pass some of the values, the assert would fail.

    TestSubscriber<Integer> ts = TestSubscriber.create();
    Observable.just(1,2,3,4).subscribe(ts);
    ts.assertValues(1,2,3); // Fail

`assertValues` uses the `equals` method when doing asserts. This lets you easily test classes that are treated as data. 

    TestSubscriber<Object> ts = TestSubscriber.create();
    Observable.just(new Object(), new Object()).subscribe(ts);
    ts.assertValues(new Object(), new Object()); // Fail

This example shows a class that has a equals defined and asserting the values from the Observable.

    public class Room {
    
        public String floor;
        public String number;

        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (o instanceof Room) {
                Room that = (Room) o;
                return (this.floor.equals(that.floor))
                        && (this.number.equals(that.number));
            }
            return false;
        }
    }

    TestSubscriber<Room> ts = TestSubscriber.create();
    Observable.just(new Room("1", "10")).subscribe(ts);
    ts.assertValue(new Room("1", "10"); // Success

Also take note that we use the shorter `assertValue` because we only need to check for one item.

# Getting all events

If need be you can also ask for all the events as a list.

    TestSubscriber<Integer> ts = TestSubscriber.create();
    Observable.just(1,2,3,4).subscribe(ts);
    List<Integer> onNextEvents = ts.getOnNextEvents();
    List<Throwable> onErrorEvents = ts.getOnErrorEvents();
    List<Notification<Integer>> onCompletedEvents = ts.getOnCompletedEvents();

## Asserting on events

If you want to do more extensive tests on your events, you can combine `getOnNextEvents` (or `getOn*Events`) with your favorite assertion library:

    Observable<Integer> obs = Observable.just(1,2,3,4)
        .filter( x -> x % 2 == 0);
    
    // note that we instanciate TestSubscriber via the constructor here 
    TestSubscriber<Integer> ts = new TestSubscriber();
    obs.subscribe(ts);

    // Note that we are not using Observable#forEach here
    // but java.lang.Iterable#forEach.
    // You should never use Observable#forEach unless you know
    // exactly what you're doing
    ts.getOnNextEvents()
        .forEach( integer -> assertTrue(integer % 2 == 0));

# Testing `Observable#error`

You can make sure that the correct exception class is emitted:

    Observable<Integer> obs = Observable.error(new Exception("I am a Teapot"));

    TestSubscriber<Integer> ts = new TestSubscriber<>();
    obs.subscribe(ts);
    
    ts.assertError(Exception.class);

You can also make sure that the exact Exception was thrown:

    Exception e = new Exception("I am a Teapot");
    Observable<Integer> obs = Observable.error(e);

    TestSubscriber<Integer> ts = new TestSubscriber<>();
    obs.subscribe(ts);
    
    ts.assertError(e);

