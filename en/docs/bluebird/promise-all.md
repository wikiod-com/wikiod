---
title: "Promise.all"
slug: "promiseall"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

```
Promise.all(
  Iterable<any> | Promise<Iterable<any>> input
) -> Promise
```

This method is useful for when you want to wait for more than one promise to complete.

Given an [`Iterable`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols)\(arrays are `Iterable`\), or a promise of an `Iterable`, which produces promises (or a mix of promises and values), iterate over all the values in the `Iterable` into an array and return a promise that is fulfilled when all the items in the array are fulfilled. The promise's fulfillment value is an array with fulfillment values at respective positions to the original array. If any promise in the array rejects, the returned promise is rejected with the rejection reason.


```js
var files = [];
for (var i = 0; i < 100; ++i) {
    files.push(fs.writeFileAsync("file-" + i + ".txt", "", "utf-8"));
}
Promise.all(files).then(function() {
    console.log("all the files were created");
});
```


This method is compatible with [`Promise.all`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all) from native promises.
</markdown></div>



## Waiting for two things to happen
```js
var firstItem = fetch("/api1").then(x => x.json());
var secondItem = fetch("/api2").then(x => x.json());
Promise.all([firstItem, secondItem]).spread((first, second) => {
  // access both results here, both requests completed at this point
});
```

