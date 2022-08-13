---
title: "Common recipes"
slug: "common-recipes"
draft: false
images: []
weight: 9892
type: docs
toc: true
---

A collection of common use cases and their implementation in RxJS.

## Caching HTTP responses
A typical use case for RxJS is creating HTTP requests and caching their results for some period of time. Also, we always want to run only one request at a time and share its response.

For example the following code caches **1** item for max. **1000ms**:

<!-- language: lang-typescript -->

    var updateRequest = Observable.defer(() => makeMockHttpRequest())
        .publishReplay(1, 1000)
        .refCount()
        .take(1);

    var counter = 1;    
    function makeMockHttpRequest() {
        return Observable.of(counter++)
            .delay(100);
    }
    
    function requestCachedHttpResult() {
        return updateRequest;
    }

Function `makeMockHttpRequest()` simulates an HTTP request that arrives with `100ms` delay.

Function `requestCachedHttpResult()` is where we subscribe to get actual or cached response. 

With `.publishReplay(1, 1000)` we used [RxJS multicasting](https://github.com/Reactive-Extensions/RxJS/blob/master/doc/api/core/operators/multicast.md) to internally use `ReplaySubject` and keep `1` item for maximum `1000ms`. Then `refCount()` is used to keep always only one subscription to the `source` which is `Observable.defer()`. This Observable is used to create a new request and increments `counter` to prove that cached values and new subscriptions share the same data.

When we want to get current data we call `requestCachedHttpResult()`. To ensure the Observer will be completed properly after emitting data we used `take(1)` operator.

<!-- language: lang-typescript -->

    requestCachedHttpResult()
        .subscribe(val => console.log("Response 0:", val));

This creates a single request with `mockDataFetch()` and prints to console:

    1

A more complicated example will call multiple requests at different times where we want to test that the mocked HTTP connections and responses are shared.

<!-- language: lang-typescript -->

    requestCachedHttpResult()
        .subscribe(val => console.log("Response 0:", val));
    
    setTimeout(() => requestCachedHttpResult()
        .subscribe(val => console.log("Response 50:", val))
    , 50);
    
    setTimeout(() => requestCachedHttpResult()
        .subscribe(val => console.log("Response 200:", val))
    , 200);
    
    setTimeout(() => requestCachedHttpResult()
        .subscribe(val => console.log("Response 1200:", val))
    , 1200);
    
    setTimeout(() => requestCachedHttpResult()
        .subscribe(val => console.log("Response 1500:", val))
    , 1500);
    
    setTimeout(() => requestCachedHttpResult()
        .subscribe(val => console.log("Response 3500:", val))
    , 3500);

See live demo: https://jsbin.com/todude/5/edit?js,console

Each request is sent with delay and happen in the following order:

`0` - First request that makes the `refCount()` to subscribe to its `source` which makes the `mockDataFetch()` call. Its response is going to be delayed by `100ms`. At this moment `ConnectableObservable` inside `publishReplay()` operator has **one** Observer.

`50` - Second request subscribes to the `ConnectableObservable` as well. At this moment `ConnectableObservable` inside `publishReplay()` operator has **two** Observer. It doesn't create another request with `makeMockHttpRequest()` because `refCount()` is already subscribed.

`100` - The first response is ready. It's first cached by the `ReplaySubject` and then reemitted to the **two** Observers subscribed to `ConnectableObservable`. Both Observers are completed thanks to `take(1)` and unsubscribed.

`200` - Subscribes to the `ReplaySubject` that immediately emits its cached value which causes `take(1)` to complete the Observer and unsubscribes right away. No HTTP requests are made and no subscription remains.

`1200` - The same as the first event at `0`. At this point the cached value has been discarded because it's older than `1000ms`.

`1500` - Same as the fourth event at `200`.

`3500` -  The same as the first event at `1200`.

The output in console is the following:

    Response 0: 1
    Response 50: 1
    Response 200: 1
    Response 1200: 2
    Response 1500: 2
    Response 3500: 3

In RxJS 5 a similar functionality was covered by `cache()` operator. However, it was removed in [`5.0.0-rc.1`](https://github.com/ReactiveX/rxjs/blob/master/CHANGELOG.md#500-rc1-2016-10-11) due to its limited functionality.

**Handling errors**

If we want to handle errors produced by the remote service (the `makeMockHttpRequest` function) we need to catch them before they're merged into the main Observable chain because any error received by the `ReplaySubject` inside `publishReplay()` would mark its internal state as `stopped` (Read more here https://www.wikiod.com/rxjs/subject#Subject and its internal state ) which is definitelly not what we want.

In the following example we're simulating an error when `counter === 2` and catching it with the `catch()` operator. We're using `catch()` to only transform the `error` notification into a regular `next` so we can handle the error in observers:

<!-- language: lang-typescript -->

    function makeMockHttpRequest() {
        return Observable.of(counter++)
            .delay(100)
            .map(i => {
                if (i === 2) {
                    throw new Error('Invalid URL');
                }
                return i;
            })
            .catch(err => Observable.of(err));
    }

See live demo: https://jsbin.com/kavihu/10/edit?js,console

This will print to console the following output. Notice the errors are received in the `next` handlers:

    Response 0: 1
    Response 50: 1
    Response 200: 1
    Response 1200: [object Error] { ... }
    Response 1500: [object Error] { ... }
    Response 3500: 3

If we want to handle errors as regular `error` notifications in each observer we have to rethrow them after the `publishReplay()` operator for the reasons explained above.

<!-- language: lang-typescript -->

    var updateRequest = Observable.defer(() => makeMockHttpRequest())
        .publishReplay(1, 1000)
        .refCount()
        .take(1)
        .map(val => {
            if (val instanceof Error) {
                throw val;
            }
            return val;
        });

See live demo: https://jsbin.com/fabosam/5/edit?js,console (notice that we had to add also error callbacks for each observer).

    Response 0: 1
    Response 50: 1
    Response 200: 1
    error callback: Error: Invalid URL
    error callback: Error: Invalid URL
    Response 3500: 3


## Sending multiple parallel HTTP requests
A very common use-case in web applications is performing multiple asynchronous (eg. HTTP) requests and gathering their results as they arrive or all of them at once (eg. in Angular2 with the [HTTP service](https://angular.io/docs/ts/latest/guide/server-communication.html)).

**1. Gathering async responses one by one as they arrive**

This is typically done with [`mergeMap()`](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#instance-method-mergeMap) operator that takes a projection function that has to return an Observable. Operator `mergeMap()` internally subscribes to each Observable immediately even if the previous Observable hasn't completed yet.

    function mockHTTPRequest(url) {
        return Observable.of(`Response from ${url}`)
          .delay(Math.random() * 1000);
    }
    
    var urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    
    Observable.from(urls)
      .mergeMap(url => mockHTTPRequest(url))
      .subscribe(val => console.log(val));

This prints responses to console in different order because of the random delay:

    Response from url-3
    Response from url-4
    Response from url-2
    Response from url-1

See live demo: https://jsbin.com/xaqudan/2/edit?js,console

Each response (item emitted via `next` call) is reemitted by `mergeMap()` immediately.

For our purpose of sending multiple HTTP requests it's useful to mention that `mergeMap()` can take three arguments in total:

1. The projection function that needs to return an Observable.
2. The result selector function that allows us to modify the result before emitting it further.
3. The number of concurrently subscribed Observables.

**Controlling the number of parallel requests**

With the third argument we can control how many parallel requests we'll handle (assuming that each Observable performing an HTTP request is "cold").

In the following example we'll run only 2 requests at the same time.

    function mockHTTPRequest(url) {
        return Observable.of(`Response from ${url}`)
          .delay(1000);
    }
    
    let urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    let start = (new Date()).getTime();
    
    Observable.from(urls)
      .mergeMap(url => mockHTTPRequest(url), undefined, 2)
      .timestamp()
      .map(stamp => [stamp.timestamp - start, stamp.value])
      .subscribe(val => console.log(val));

See live demo: https://jsbin.com/sojejal/4/edit?js,console

Notice that the first two requests completed after 1s while the other two after 2s.

    [1004, "Response from url-1"]
    [1010, "Response from url-2"]
    [2007, "Response from url-3"]
    [2012, "Response from url-4"]

**Handling errors**

If any of the source Observables fail (sends `error` notification) the `mergeMap()` resends the error further as `error`. In case we want each Observable to fail gracefully we need to use for example [`catch()`](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#instance-method-catch) operator.

    function mockHTTPRequest(url) {
      return Observable.of(`Response from ${url}`)
        .delay(Math.random() * 1000)
        .map(value => {
          if (url === 'url-3') {
            throw new Error(`Error response from ${url}`)
          }
          return value;
        });
    }
    
    var urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    
    Observable.from(urls)
      .mergeMap(url => mockHTTPRequest(url).catch(() => Observable.empty()))
      .subscribe(val => console.log(val));

The response for `url-3` throws an error that is sent as `error` notification. This is later caught by `catch()` operator and replaced with `Observable.empty()` which is just a `complete` notification. For this reason this response is ignored.

The output for this example is the following:

    Response from url-4
    Response from url-1
    Response from url-2

See live demo: https://jsbin.com/kuqumud/4/edit?js,console

**2. Gathering all async responses at once**

Following the preceding examples we could gather all responses with [`toArray()`](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#instance-method-toArray) operator.

    function mockHTTPRequest(url) {
        return Observable.of(`Response from ${url}`)
          .delay(Math.random() * 1000);
    }
    
    var urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    
    Observable.from(urls)
      .mergeMap(url => mockHTTPRequest(url))
      .toArray()
      .subscribe(val => console.log(val));

However, using `toArray()` operator has an important consequence. Whether the subscriber receives the results isn't only controlled by completing all the HTTP requests but also by completing the source Observable (`Observable.from` in our case). This means that we can't use source Observables that never complete (eg. `Observable.fromEvent`).

Another way to achieve the same result is using [`Observable.forkJoin()`](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#static-method-forkJoin) which takes as argument an array of Observables that we want to subscribe to and wait until all of them **emit at least one value and complete**.

    function mockHTTPRequest(url) {
        return Observable.of(`Response from ${url}`)
          .delay(Math.random() * 1000);
    }
    
    var urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    var observables = urls.map(url => mockHTTPRequest(url));
    
    Observable.forkJoin(observables)
      .subscribe(val => console.log(val));

This prints all responses as a single array:

    ["Response from url-1", "Response from url-2", "Response from url-3", "Response from url-4"]

See live demo: https://jsbin.com/fomoye/2/edit?js,console

The `Observable.forkJoin()` also takes as an optional argument a result selector function that lets us modify the final result before emitting it further:

    Observable.forkJoin(observables, (...results) => {
        return results.length;
      })
      .subscribe(val => console.log(val));

This prints to console:

    4

See live demo: https://jsbin.com/muwiqic/1/edit?js,console

Note that the argument for the result selector function are unpacked.

**Handling errors**

For error handling we can use the same approach as in the preceding example with `catch()` operator.

However, there's one important thing to be aware of. The `forkJoin()` requires every source Observable to emit at least one value. If we used `catch(() => Observable.empty())` like we did before the `forkJoin()` would never emit anything because `Observable.empty()` is just a `complete` notification.  
This is why we need to use for example `Observable.of(null)` which is a `null` value followed by `complete` notification.

    function mockHTTPRequest(url) {
      return Observable.of(`Response from ${url}`)
        .delay(Math.random() * 1000)
        .map(value => {
          if (url === 'url-3') {
            throw new Error(`Error response from ${url}`)
          }
          return value;
        });
    }
    
    var urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    
    var observables = urls.map(url => mockHTTPRequest(url).catch(() => Observable.of(null)));
    
    Observable.forkJoin(observables)
      .subscribe(val => console.log(val));

See live demo: https://jsbin.com/yidiked/2/edit?js,console

This prints to console: 

    ["Response from url-1", "Response from url-2", null, "Response from url-4"]

Notice that the error is replaced by `null`. If we used just `Observable.empty()` the `forkJoin()` would never emit anything.

## Discarding slow/outdated rest calls
A common usecase is to discard certain rest-calls, that are not needed any more after certain user-inputs. The most prominent example would be, when a user uses some search-function, makes a request, makes another request and for some reason the first request arrives after the second request and the application displays the outdated data of the old request.

This is a perfect usecase for the `switchMap`-operator.

    searchInput$
        .switchMap(() => makeRestCall());

In this case the stream will `switch` to the rest-call, but only until new data in emitted on the `searchInput$`, then the stream inside the `switchMap` is discarded and a new rest-call is made. So a rest-result will only be considered if it finished before the next click.

And here is a fully fledged mocked example:

    // some initial data-mocking
    const Observable = Rx.Observable;
    var counter = 1;
    function mockDataFetch() {
        return Observable.of(counter++)
            .delay(500);
    }
    
    // the recipe
    
    const searchInput$ = new Rx.Subject();
    searchInput$
        .do(searchInput => console.log("Searching for " + searchInput))
        .switchMap(searchInput => mockDataFetch()
                  .map(result => ({result, searchInput}))
         )
        .do(data => console.log("Received result for " + data.searchInput + ": " + data.result))
        .subscribe();
    
    // simulating search inputs
    searchInput$.next("google");
    setTimeout(() => searchInput$.next("froogle"), 600);
    setTimeout(() => searchInput$.next("doodle"), 800);
    setTimeout(() => searchInput$.next("poodle"), 1000);
    setTimeout(() => searchInput$.next("noodle"), 1600);

See live demo: https://jsbin.com/suzakahoro/1/edit?js,console

## Sending multiple sequential HTTP requests
Making a sequence of HTTP requests has two primary reasons:

- Requests are depending on each other (the result from one requests is required for a consecutive request).

- We want to spread server load into multiple requests.

**1. Making multiple dependent requests**

This can be performed using the [`concatMap()`](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#instance-method-concatMap) operator to transform one response to parameters required for the consecutive request.

    function mockHTTPRequest(url) {
        return Observable.of(`Response from ${url}`)
          .delay(1000);
    }
    
    function timestamp() {
      return (new Date()).getTime() - start;
    }
    
    var start = (new Date()).getTime();
    
    Observable.of('url-1') 
      // first request
      .concatMap(url => {
        console.log(timestamp() + ': Sending request to ' + url);
        return mockHTTPRequest(url);
      })
      .do(response => console.log(timestamp() + ': ' + response))
    
      // second request
      .concatMap(response => {
        console.log(timestamp() + ': Sending request to ' + response);
        let newUrl = 'url-' + response.length; // create new requests url
        return mockHTTPRequest(newUrl);
      })
      .do(response => console.log(timestamp() + ': ' + response))
    
      // third request
      .concatMap(response => {
        console.log(timestamp() + ': Sending request to ' + response);
        let newUrl = 'url-' + response.length; // create another requests url
        return mockHTTPRequest(newUrl);
      })
      .subscribe(response => console.log(timestamp() + ': ' + response));

Operator `concatMap()` internally subscribes to the Observable returned the from its projection function and waits until it completes while re-emitting all its values.

This example timestamps each request and response:

    3: Sending request to url-1
    1010: Response from url-1
    1011: Sending request to Response from url-1
    2014: Response from url-19
    2015: Sending request to Response from url-19
    3017: Response from url-20

See live demo: https://jsbin.com/fewidiv/6/edit?js,console

**Handling errors**

If any of the HTTP requests fail we obviously don't want to continue because we're not able to construct the following request.

**2. Making consecutive requests**

In case we're not interested in the response from the previous HTTP request we can take just an array of URLs and execute them one after another.

    function mockHTTPRequest(url) {
        return Observable.of(`Response from ${url}`)
          .delay(1000);
    }
    
    let urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    let start = (new Date()).getTime();
    
    Observable.from(urls)
      .concatMap(url => mockHTTPRequest(url))
      .timestamp()
      .map(stamp => [stamp.timestamp - start, stamp.value])
      .subscribe(val => console.log(val));

This example prints timestamped responses:

    [1006, "Response from url-1"]
    [2012, "Response from url-2"]
    [3014, "Response from url-3"]
    [4016, "Response from url-4"]

See live demo: https://jsbin.com/kakede/3/edit?js,console

**Delaying consecutive calls**

We might also want to make a small delay between each request. In such case we need to append `delay()` after each `mockHTTPRequest()` call.

    Observable.from(urls)
      .concatMap(url => {
        return mockHTTPRequest(url)
          .do(response => console.log(((new Date()).getTime() - start) + ': Sending request to ' + url))
          .delay(500);
      })
      .timestamp()
      .map(stamp => [stamp.timestamp - start, stamp.value])
      .subscribe(val => console.log(val));

This prints to console the following output:

    2024: Sending request to url-1
    [2833, "Response from url-1"]
    4569: Sending request to url-2
    [5897, "Response from url-2"]
    7880: Sending request to url-3
    [8674, "Response from url-3"]
    9789: Sending request to url-4
    [10796, "Response from url-4"]

See live demo: https://jsbin.com/kakede/4/edit?js,console

**Handling errors**

If we simply want to ignore when any of the HTTP requests fail we have to chain `catch()` ofter each `mockHTTPRequest()`.

    function mockHTTPRequest(url) {
      if (url == 'url-3') {
        return Observable.throw(new Error(`Request ${url} failed.`));
      } else {
        return Observable.of(`Response from ${url}`)
          .delay(1000);
      }
    }
    
    let urls = ['url-1', 'url-2', 'url-3', 'url-4'];
    let start = (new Date()).getTime();
    
    Observable.from(urls)
      .concatMap(url => mockHTTPRequest(url).catch(obs => Observable.empty()))
      .timestamp()
      .map(stamp => [stamp.timestamp - start, stamp.value])
      .subscribe(val => console.log(val));

This simply ignores the `url-3` call:

    [1004, "Response from url-1"]
    [2012, "Response from url-2"]
    [3016, "Response from url-4"]

See live demo: https://jsbin.com/jowiqo/2/edit?js,console

If we didn't use the `catch()` operator the `url-3` would cause the chain to send  an error notification and the last `url-4` wouldn't be executed.

See live demo: https://jsbin.com/docapim/3/edit?js,console


## Rate limiting
A common problem with remote services is rate limiting. The remote service allows us to send only a limited number of requests or amount of data per time period.

In RxJS 5 a very similar functionality is provided by the [`bufferTime`](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#instance-method-bufferTime) operator and especially if we leave the second parameter unspecified (it defines how often we want to create a new buffer. If we leave it undefined/null it'll create a new buffer right after emitting the current one).

A typical usage of `bufferTime` will look like this:

    bufferTime(1000, null, 5)

This will buffer items until one of the two conditions are met. Then it'll emit the buffer and start another one:

- the operator has been collecting items for `1000ms`
- the operator has already collected `5` items

For demonstrational purposes we can create a source Observable that emits very fast so the `bufferTime` will hit the size limit (`5`) and emit more often than once every `1000ms`:

    const source = Observable.range(1, 25)
      .concatMap(val => Observable.of(val).delay(75));

Then we'll chain it with `bufferTime` and `concatMap`. The `concatMap` operator is where we force the `1000ms` delay:

    const startTime = (new Date()).getTime();

    const source = Observable.range(1, 25)
      .concatMap(val => Observable.of(val).delay(75));
    
    source.bufferTime(1000, null, 5)
      .concatMap(buffer => Observable.of(buffer).delay(1000))
      .timestamp()
      .map(obj => {
        obj.timestamp = obj.timestamp - startTime;
        return obj;
      })
      .subscribe(obj => console.log(obj));

See live demo: https://jsbin.com/kotibow/3/edit?js,console

We added also `timestamp()` to see the emission times to make sure the delay is really at least `1000ms`.

Note that we didn't have to use `Observable.of(buffer)` at all. We're using it here just to manually check that the number of buffered items is correct.

From the console output we can see that the delay between two emissions is roughly `1000ms`:

    Timestamp { value: [ 1, 2, 3, 4, 5 ], timestamp: 1475 }
    Timestamp { value: [ 6, 7, 8, 9, 10 ], timestamp: 2564 }
    Timestamp { value: [ 11, 12, 13, 14, 15 ], timestamp: 3567 }
    Timestamp { value: [ 16, 17, 18, 19, 20 ], timestamp: 4572 }
    Timestamp { value: [ 21, 22, 23, 24, 25 ], timestamp: 5573 }
    Timestamp { value: [], timestamp: 6578 }

Now we can also test a situation where the source emits slowly so the `bufferTime` operator is going to hit the max interval condition:

    const source = Observable.range(1, 25)
      .concatMap(val => Observable.of(val).delay(300));

See live demo: https://jsbin.com/tuwowan/2/edit?js,console

Then the output should start after about `2s` because it took `1s` for the `bufferTime` operator to emit and then we added the `1s` delay;

    Timestamp { value: [ 1, 2, 3 ], timestamp: 2017 }
    Timestamp { value: [ 4, 5, 6 ], timestamp: 3079 }
    Timestamp { value: [ 7, 8, 9, 10 ], timestamp: 4088 }
    Timestamp { value: [ 11, 12, 13 ], timestamp: 5093 }
    Timestamp { value: [ 14, 15, 16 ], timestamp: 6094 }
    Timestamp { value: [ 17, 18, 19, 20 ], timestamp: 7098 }
    Timestamp { value: [ 21, 22, 23 ], timestamp: 8103 }
    Timestamp { value: [ 24, 25 ], timestamp: 9104 }

If we wanted to use this approach in a real world application we'd put the remote call into the `concatMap` operator. This way we can control whether we want to force the `1s` delay between requests or responses from the remote service.

For example we can force the minimum `1s` delay between requests by using `forkJoin` in the `concatMap` callback:

    function mockHTTPRequest(buffer) {
      return Observable.of(true).delay(Math.random() * 1500)
    }
    
    const startTime = (new Date()).getTime();
    const source = Observable.range(1, 25)
      .concatMap(val => Observable.of(val).delay(75));
    
    source.bufferTime(1000, null, 5)
      .concatMap(buffer => Observable.forkJoin(
        mockHTTPRequest(buffer),
        Observable.of(buffer).delay(1000)
      ))
      .timestamp()
      .map(obj => {
        obj.timestamp = obj.timestamp - startTime;
        return obj;
      })
      .subscribe(obj => console.log(obj));

See live demo: https://jsbin.com/xijaver/edit?js,console

Thanks to `forkJoin` the `concatMap` needs to wait for both Observables to complete.

On the other hand, if we wanted to force `1s` delay between responses we'd just append the `delay()` operator after `mockHTTPRequest()`:

    .concatMap(buffer => mockHTTPRequest(buffer).delay(1000))

See live demo: https://jsbin.com/munopot/2/edit?js,console

