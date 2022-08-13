---
title: "Converting a callback API to promises."
slug: "converting-a-callback-api-to-promises"
draft: false
images: []
weight: 9849
type: docs
toc: true
---

Promises have state, they start as pending and can settle to:

 - __fulfilled__ meaning that the computation completed successfully.
 - __rejected__ meaning that the computation failed.

Promise returning functions _should never throw_, they should return rejections instead. Throwing from a promise returning function will force you to use both a `} catch { ` _and_ a `.catch`. People using promisified APIs do not expect promises to throw. If you're not sure how async APIs work in JS - please [see this answer](http://stackoverflow.com/questions/14220321/how-to-return-the-response-from-an-asynchronous-call/16825593#16825593) first.

## Converting a whole NodeJS module at once
Let's say you have a library that returns callbacks, for example the `fs` module in NodeJS:

```js
const fs = require("fs");
fs.readFile("/foo.txt", (err, data) => {
  if(err) throw err;
  console.log(data);
});
```

We want to convert it to a promise returning API, with bluebird - we can do this using [`promisifyAll`](http://bluebirdjs.com/docs/api/promise.promisifyall.html) which converts an entire API to use promises:


```js
const Promise = require("bluebird");
const fs = Promise.promisifyAll(require("fs"));
// this automatically adds `Async` postfixed methods to `fs`.
fs.readFileAsync("/foo.txt").then(console.log);
```

Which lets you use the whole module as promises.

Here are some common ecxamples on how to promisify certain modules:
```js
// The most popular redis module
var Promise = require("bluebird");
Promise.promisifyAll(require("redis"));
// The most popular mongodb module
var Promise = require("bluebird");
Promise.promisifyAll(require("mongodb"));
// The most popular mysql module
var Promise = require("bluebird");
// Note that the library's classes are not properties of the main export
// so we require and promisifyAll them manually
Promise.promisifyAll(require("mysql/lib/Connection").prototype);
Promise.promisifyAll(require("mysql/lib/Pool").prototype);
// Mongoose
var Promise = require("bluebird");
Promise.promisifyAll(require("mongoose"));
// Request
var Promise = require("bluebird");
Promise.promisifyAll(require("request"));
// Use request.getAsync(...) not request(..), it will not return a promise
// mkdir
var Promise = require("bluebird");
Promise.promisifyAll(require("mkdirp"));
// Use mkdirp.mkdirpAsync not mkdirp(..), it will not return a promise
// winston
var Promise = require("bluebird");
Promise.promisifyAll(require("winston"));
// rimraf
var Promise = require("bluebird");
// The module isn't promisified but the function returned is
var rimrafAsync = Promise.promisify(require("rimraf"));
// xml2js
var Promise = require("bluebird");
Promise.promisifyAll(require("xml2js"));
// jsdom
var Promise = require("bluebird");
Promise.promisifyAll(require("jsdom"));
// fs-extra
var Promise = require("bluebird");
Promise.promisifyAll(require("fs-extra"));
// prompt
var Promise = require("bluebird");
Promise.promisifyAll(require("prompt"));
// Nodemailer
var Promise = require("bluebird");
Promise.promisifyAll(require("nodemailer"));
// ncp
var Promise = require("bluebird");
Promise.promisifyAll(require("ncp"));
// pg
var Promise = require("bluebird");
Promise.promisifyAll(require("pg"));
```

## Converting a single NodeJS function
You can convert a single function with a callback argument to a `Promise`-returning version with [`Promise.promisify`](http://bluebirdjs.com/docs/api/promise.promisify.html), so this:

```js
const fs = require("fs");
fs.readFile("foo.txt", (err, data) => {
   if(err) throw err;
   console.log(data);
});
```

becomes:

```js
const promisify = require("bluebird");
const readFile = promisify(require("fs").readFile));
readFile("foo.txt").then(console.log); // promisified version
```

## Converting any other callback API
In order to convert any callback API to promises assuming the `promisify` and `promisifyAll` version doesn't fit - you can use the [promise constructor](http://bluebirdjs.com/docs/api/new-promise.html).

Creating promises generally means specifying when they settle - that means when they move to the fulfilled (completed) or rejected (errored) phase to indicate the data is available (and can be accessed with `.then`).


```js
new Promise((fulfill, reject) => { // call fulfill/reject to mark the promise
   someCallbackFunction((data) => {
      fulfill(data); // we mark it as completed with the value
   })
});
```

As an example, let's convert `setTimeout` to use promises:

```js
function delay(ms) { // our delay function that resolves after ms milliseconds
  return new Promise((resolve, reject) => { // return a new promise
    setTimeout(resolve, ms); // resolve it after `ms` milliseconds have passed
  })
}
// or more concisely:
const delay = ms => new Promise(r => setTimeout(r, ms));
```

We can now use it like a regular promise returning function:

```js
delay(1000).then(() => console.log("One second passed")).
            then(() => delay(1000)).
            then(() => console.log("Another second passed"));
```

