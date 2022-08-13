---
title: "Operators"
slug: "operators"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

This document describes the basic behaviour of an operator.

## Operators, an introduction
An operator can be used to manipulate the flow of objects from `Observable` to `Subscriber`. 

    Observable<Integer> integerObservable = Observable.just(1, 2, 3); // creating a simple Integer observable
    Subscriber<String> mSubscriber = new Subscriber<String>() {
        @Override
        public void onCompleted() {
            System.out.println("onCompleted called!");
        }

        @Override
        public void onError(Throwable throwable) {
            System.out.println("onError called");
        }

        @Override
        public void onNext(String string) {
            System.out.println("onNext called with: " + string);
        }
    }; // a simple String subscriber

    integerObservable
        .map(new Func1<Integer, String>() {
            @Override
            public String call(Integer integer) {
                switch (integer) {
                    case 1:
                        return "one";
                    case 2:
                        return "two";
                    case 3:
                        return "three";
                    default:
                        return "zero";
                }
            }
    }).subscribe(mSubscriber);
<br>The output would be:

    onNext called with: one
    onNext called with: two
    onNext called with: three
    onCompleted called!

The `map`operator changed the `Integer` observable to a `String` observable, thereby manipulating the flow of objects.

**Operator Chaining**

Multiple operators can be `chained` together to for more powerful transforms and manipulations.

    integerObservable // emits 1, 2, 3
                .map(i -> i + 10) // adds 10 to each item; emits 11, 12, 13
                .filter(i -> i > 11) // emits items that satisfy condition; 12, 13
                .last() // emits last item in observable; 13
                // unlimited operators can be added ...
                .subscribe(System.out::println); // prints 13

Any number of operators can be added in between the `Observable` and `Subscriber`. 

## flatMap Operator
The `flatMap` operator help you to transform one event to another `Observable` (or transform an event to zero, one, or more events).

It's a perfect operator when you want to call another method which return an `Observable`

     public Observable<String> perform(int i) {
          // ...
     }

     Observable.just(1, 2, 3)
               .flatMap(i -> perform(i))
               .subscribe(result -> System.out.println("result ->" + result);

`flatMap` will serialize `perform` subscriptions __but__ events emited by `perform` may not be ordered. So you may receive events emitted by the last perform call __before__ events from the first `perform` call (you should use `concatMap` instead).

If your creating another `Observable` in your subscriber, you __should__ use `flatMap` instead. The main idea is : __never leave the Observable__

For example :
 
     Observable.just(1, 2, 3)
               .subscribe(i -> perform(i));

can easily be replaced by : 

     Observable.just(1, 2, 3)
               .flatMap(i -> perform(i))
               .subscribe();

Reactivex.io documentation : http://reactivex.io/documentation/operators/flatmap.html

## filter Operator
You can use the `filter` operator to filter out items from the values stream based on a result of a predicate method. 

In other words, the items passing from the Observer to the Subscriber will be discarded based on the Function you pass `filter`, if the function returns `false` for a certain value, that value will be filtered out.

**Example:**

````
List<Integer> integers = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

Observable.from(integers)
    .filter(number -> {
        return (number  % 2 == 0); 
        // odd numbers will return false, that will cause them to be filtered 
    })
    .map(i -> {
        return Math.pow(i, 2); // take each number and multiply by power of 2
    }) 
    .subscribe(onNext -> {
         System.out.println(onNext); // print out the remaining numbers
    });
````

This code will print out 
````
0.0
4.0
16.0
36.0
64.0
````

## map Operator
You can use the `map` operator to map the values of a stream to different values based on the outcome for each value from the function passed to `map`. The outcome stream is a new copy and will not modify the provided stream of values, the result stream will have the same length of the input stream but may be of different types.

The function passed to `.map()`, must return a value.

**Example:**

````
List<Integer> numbers = Arrays.asList(1, 2, 3);
Observable.from(numbers)
    .map(number -> {
        return number.toString(); // convert each integer into a string and return it
    }) 
    .subscribe(onNext -> {
         System.out.println(onNext); // print out the strings
    });
````
This code will print out
````
"1"
"2"
"3"
````
In this example the Observable accepted a `List<Integer>` the list will be transformed to a `List<String>` in the pipeline and the `.subscribe` will emit `String`'s

## doOnNext operator
``doOnNext`` operator called every time when source `Observable` emits an item. It can be used for debugging purposes, applying some action to the emitted item, logging, etc...

    Observable.range(1, 3)
        .doOnNext(value -> System.out.println("before transform: " + value))
        .map(value -> value * 2)
        .doOnNext(value -> System.out.println("after transform: " + value))
        .subscribe();

In the example below `doOnNext` is never called because the source `Observable` emits nothing because `Observable.empty()` calls `onCompleted` after subscribing.

    Observable.empty()
        .doOnNext(item -> System.out.println("item: " + item))
        .subscribe();




## repeat operator
`repeat` operator allow to repeat whole sequence from source `Observable`. 

    Observable.just(1, 2, 3)
        .repeat()
        .subscribe(
            next -> System.out.println("next: " + next),
            error -> System.out.println("error: " + error),
            () -> System.out.println("complete")
        );

Output of the example above

    next: 1
    next: 2
    next: 3
    next: 1
    next: 2
    next: 3

This sequence repeats infinite number of times and never completes. 

To repeat sequence finite number of times just pass integer as an argument to `repeat` operator.

    Observable.just(1, 2, 3)
        // Repeat three times and complete
        .repeat(3)
        .subscribe(
            next -> System.out.println("next: " + next),
            error -> System.out.println("error: " + error),
            () -> System.out.println("complete")
        );

This example prints

    next: 1
    next: 2
    next: 3
    next: 1
    next: 2
    next: 3
    next: 1
    next: 2
    next: 3
    complete

It is very important to understand that `repeat` operator resubscribes to source `Observable` when source `Observable` sequence completes. Let's rewrite example above using `Observable.create`.

    Observable.<Integer>create(subscriber -> {

        //Same as Observable.just(1, 2, 3) but with output message
        System.out.println("Subscribed");
        subscriber.onNext(1);
        subscriber.onNext(2);
        subscriber.onNext(3);
        subscriber.onCompleted();
    })
            .repeat(3)
            .subscribe(
                    next -> System.out.println("next: " + next),
                    error -> System.out.println("error: " + error),
                    () -> System.out.println("complete")
            );
This example prints

    Subscribed
    next: 1
    next: 2
    next: 3
    Subscribed
    next: 1
    next: 2
    next: 3
    Subscribed
    next: 1
    next: 2
    next: 3
    complete

When using operator chaining it is important to know that `repeat` operator repeats **whole sequence** rather than preceding operator.

    Observable.<Integer>create(subscriber -> {
        System.out.println("Subscribed");
        subscriber.onNext(1);
        subscriber.onNext(2);
        subscriber.onNext(3);
        subscriber.onCompleted();
    })
            .map(value -> value * 2) //First chain operator
            .map(value -> "modified " + value) //Second chain operator
            .repeat(3)
            .subscribe(
                    next -> System.out.println("next: " + next),
                    error -> System.out.println("error: " + error),
                    () -> System.out.println("complete")
            );

This example prints

    Subscribed
    next: modified 2
    next: modified 4
    next: modified 6
    Subscribed
    next: modified 2
    next: modified 4
    next: modified 6
    Subscribed
    next: modified 2
    next: modified 4
    next: modified 6
    complete

This example shows that `repeat` operator repeats whole sequence resubscribing to `Observable` rather than repeating last `map` operator and it doesn't matter in which place in the sequence `repeat` operator used.

This sequence 

    Observable.<Integer>create(subscriber -> {
            //...
        })
        .map(value -> value * 2) //First chain operator
        .map(value -> "modified " + value) //Second chain operator
        .repeat(3)
        .subscribe(
            /*....*/
        );

is equal to this sequence

    Observable.<Integer>create(subscriber -> {
            //...
        })
        .repeat(3)
        .map(value -> value * 2) //First chain operator
        .map(value -> "modified " + value) //Second chain operator
        .subscribe(
            /*....*/
        );






