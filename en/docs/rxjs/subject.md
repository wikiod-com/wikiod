---
title: "Subject"
slug: "subject"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Subjects are classes that behave as Observables and observers at the same time.

http://reactivex.io/documentation/subject.html

## Subject and its internal state
In Rx Subjects have internal states that can control their behavior.

A common use-case form Subject is subscribing it to multiple Observables. The following example creates two different Observables and subscribes a Subject to both of them. Then it tries to print all values that went through:

    let subject = new Subject();
    subject.subscribe(val => console.log(val));
    
    Observable.range(1, 5).subscribe(subject);
    Observable.from(['a', 'b', 'c']).subscribe(subject);

See live demo: https://jsbin.com/pesumup/2/edit?js,console

This example just prints numbers `1 - 5` and didn't print any of the characters `a`, `b`, `c`.

    1
    2
    3
    4
    5

The question is what happened? The problem here is the internal state of the Subject instance when it received the `complete` notification. When a Subject receives an `error` or `complete` notifications it [marks itself as stopped](https://github.com/ReactiveX/rxjs/blob/master/src/Subject.ts#L86) and [will never emit any other signal](https://github.com/ReactiveX/rxjs/blob/master/src/Subject.ts#L56).

It needs to be this way because Subjects are basically Observables and Observables can only emit one `complete` or `error` notification at the end of the stream but never both.

The problem with the example above is that the first Observable `Observable.range()` emits also the `complete` notification which is then received by the Subject and therefore it [doesn't reemit any value when subscribed](https://github.com/ReactiveX/rxjs/blob/master/src/Subject.ts#L116) to the second Observable.

We can see that the Subject really receives the `complete` notification by setting also the complete callback.

    subject.subscribe(val => console.log(val), null, () => console.log('complete'));

The output is the same just at the end it also prints `complete`.

    1
    2
    3
    4
    5
    complete

So if we don't want the Subject to receive the `complete` notification, we can just manually send the `next` signals. This means instead of subscribing the Subject directly we'll subscribe a callback that calls the `next()` method on the Subject:

    Observable.range(1, 5).subscribe(val => subject.next(val));
    Observable.from(['a', 'b', 'c']).subscribe(val => subject.next(val));

See live demo: https://jsbin.com/funeka/1/edit?js,console

    1
    2
    3
    4
    5
    a
    b
    c

Note that this exact same principle applies everywhere where we use Subjects.

For example operators such as `publish()`, `share()` and all their variants that use the same instance of Subject under the hood are affected by this.

