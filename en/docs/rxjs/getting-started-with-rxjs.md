---
title: "Getting started with rxjs"
slug: "getting-started-with-rxjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Using a CDN:
===========

<!-- language: lang-html -->
    <!DOCTYPE html>
    <head>
      <script src="https://cdn.jsdelivr.net/rxjs/4.1.0/rx.min.js"></script>
    </head>
    <body>

      <script>
        // `Rx` is available
        var one$ = Rx.Observable.of(1);
        var onesub = one$.subscribe(function (one) {
          console.log(one); // 1
        });
        // alternatively: subscribe(console.log)
      </script>
    </body>
    </html>

CDN if using [RxJS 5 (RC)](https://github.com/ReactiveX/rxjs#rxjs-5-beta):

<!-- language: lang-html -->

    <script src="https://npmcdn.com/@reactivex/rxjs@5.0.0-rc.1/dist/global/Rx.js"></script>


Using a bundler:
================

First install into your project directory with [npm](http://npmjs.com):

    npm install rx

Or using [RxJS 5 (RC)](https://github.com/ReactiveX/rxjs#rxjs-5-beta):

    npm install rxjs

Then, in your JavaScript file:

<!-- language: lang-js -->
    var Rx = require('rx');
    //var Rx = require('rxjs/Rx'); // v5beta 

    var one$ = Rx.Observable.of(1);
    var onesub = one$.subscribe(function (one) {
      console.log(one); // 1
    });

If using an es6/2015 compatible bundler:

<!-- language: lang-js -->
    import Rx from 'rx';
    //import Rx from 'rxjs/Rx'; // v5beta

    const one$ = Rx.Observable.of(1);
    const onesub = one$.subscribe(one => console.log(one)); // 1
    



