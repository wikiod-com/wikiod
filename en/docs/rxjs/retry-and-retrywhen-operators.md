---
title: "Retry and RetryWhen Operators"
slug: "retry-and-retrywhen-operators"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Retry and RetryWhen can be used to attempt to recover Observables that might have errors in their stream.

## Syntax
 1. .retry(n: number): Observable
    * n: retry will attempt the source Observable this many times.
 2. .retryWhen(receives: notificationHandler, the: scheduler): Observable
    * receives: an Observable of notifications which the use can complete or error.
      * If the 'receives' Observable returns cleanly
    (completes) the source Observable will be reattempted.
      * If the 'receives' Observable returns an error, the source Observable is
    aborted.
    * scheduler: The source Observable is subscribed to this scheduler.

## Retry with backoff, until success or max number of attempts reached
The following code will attempt to execute `loadFromHttp()` up to 5 times (`maxAttempts`), with each attempt delayed by as many seconds. If `maxAttempts` is surpassed, the Observable gives up.

```
    // assume loadFromHttp() returns a Promise, which might fail.
    Rx.Observable.from(loadFromHttp())
    .retryWhen((attempts) => {
        let maxAttempts = 5;

        Rx.Observable.range(1, maxAttempts+1).zip(attempts, (i, attempt) => [i, attempt])
        .flatMap(([i, attempt]) => {
            if (i <= maxAttempts) {
                console.log(`Retrying in ${i} second(s)`);
                return Rx.Observable.timer(i * 1000);
            } else {
                throw attempt;
            }
        })
    })
```

