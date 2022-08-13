---
title: "Operator PublishReplay"
slug: "operator-publishreplay"
draft: false
images: []
weight: 9939
type: docs
toc: true
---

## How does PublishReplay work
It internally creates a [`ReplaySubject`][2] and makes it `multicast` compatible. The minimal replay value of `ReplaySubject` is 1 emission. This results in the following:

- First subscription will trigger the `publishReplay(1)` to internally subscribe to the source stream and pipe all emissions through the `ReplaySubject`, effectively caching the last _n_(=1) emissions
- If a second subscription is started while the source is still active the `multicast()` will connect us to the same `replaySubject` and we will receive all next emissions until the source stream completes.
- If a subscription is started after the source is already completed the replaySubject has cached the last _n_ emissions and it will only receive those before completing.

<!-- begin snippet: js hide: false console: true babel: false -->

<!-- language: lang-js -->

    const source = Rx.Observable.from([1,2])
      .mergeMap(i => Rx.Observable.of('emission:'+i).delay(i * 100))
      .do(null,null,() => console.log('source stream completed'))
      .publishReplay(1)
      .refCount();

    // two subscriptions which are both in time before the stream completes
    source.subscribe(val => console.log(`sub1:${val}`), null, () => console.log('sub1 completed'));
    source.subscribe(val => console.log(`sub2:${val}`), null, () => console.log('sub2 completed'));

    // new subscription after the stream has completed already
    setTimeout(() => {
      source.subscribe(val => console.log(`sub_late-to-the-party:${val}`), null, () => console.log('sub_late-to-the-party completed'));
    }, 500);


<!-- language: lang-html -->

    <script src="https://cdnjs.cloudflare.com/ajax/libs/rxjs/5.0.3/Rx.js"></script>


<!-- end snippet -->

  [1]: https://github.com/ReactiveX/rxjs/blob/master/src/operator/publishReplay.ts
  [2]: https://github.com/ReactiveX/rxjs/blob/master/src/ReplaySubject.ts


## Unexpected emissions when using publishReplay
Based on a [question][1]. The following snippets Does not cache the expected emission and prevents further calls. Instead it re-subscribes to the _realSource_ for every subscription.

<!-- begin snippet: js hide: false console: true babel: false -->

<!-- language: lang-js -->

    var state = 5
    var realSource = Rx.Observable.create(observer =>  {
      console.log("creating expensive HTTP-based emission"); 
      observer.next(state++);
    //  observer.complete(); //absent on purpose
      
      return () => {
        console.log('unsubscribing from source')
      }
    });


    var source = realSource
    .do(null, null, () => console.log('stream completed'))
    .publishReplay()
    .refCount();
        
    subscription1 = source.subscribe({next: (v) => console.log('observerA: ' + v)});
    subscription1.unsubscribe();
 
    subscription2 = source.subscribe(v => console.log('observerB: ' + v));
    subscription2.unsubscribe();
            
<!-- language: lang-html -->

    <script src="https://cdnjs.cloudflare.com/ajax/libs/rxjs/5.1.0/Rx.js"></script>


<!-- end snippet -->


When running this snippet we can see clearly that it is not emitting duplicate values for _Observer B_, it is in fact creating new emissions for every subscription. How come?

Every subscription is unsubscribed before the next subscription takes place. This effectively makes the refCount decrease back to zero, no multicasting is being done.

The issue resides in the fact that the `realSource` stream *does not complete*. Because we are not multicasting the next subscriber gets a fresh instance of `realSource` through the ReplaySubject and the new emissions are prepended with the previous already emitted emissions. 


  [1]: https://stackoverflow.com/questions/42189801/rxjs-5-publishreplay-refcount

