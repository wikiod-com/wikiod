---
title: "RXJS and Observables"
slug: "rxjs-and-observables"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Wait for multiple requests
One common scenario is to wait for a number of requests to finish before continuing. This can be accomplished using the [`forkJoin` method](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html#static-method-forkJoin).

In the following example, `forkJoin` is used to call two methods that return `Observables`. The callback specified in the `.subscribe` method will be called when both Observables complete. The parameters supplied by `.subscribe` match the order given in the call to `.forkJoin`. In this case, first `posts` then `tags`.

```
loadData() : void {
    Observable.forkJoin(
        this.blogApi.getPosts(),
        this.blogApi.getTags()
    ).subscribe((([posts, tags]: [Post[], Tag[]]) => {
        this.posts = posts;
        this.tags = tags;
    }));
}
```

## Basic Request

The following example demonstrates a simple HTTP GET request. `http.get()` returns an `Observable` which has the method `subscribe`. This one appends the returned data to the `posts` array.

```
var posts = []

getPosts(http: Http): {
    this.http.get(`https://jsonplaceholder.typicode.com/posts`)
        .subscribe(response => {
            posts.push(response.json());
        });
}
```

