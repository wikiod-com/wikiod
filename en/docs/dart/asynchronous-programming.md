---
title: "Asynchronous Programming"
slug: "asynchronous-programming"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Converting callbacks to Futures
Dart has a robust async library, with [Future][1], [Stream][2], and more. However, sometimes you might run into an asynchronous API that uses _callbacks_ instead of _Futures_. To bridge the gap between callbacks and Futures, Dart offers the _Completer_ class. You can use a Completer to convert a callback into a Future.


Completers are great for bridging a callback-based API with a Future-based API. For example, suppose your database driver doesn't use Futures, but you need to return a Future. Try this code: 

      // A good use of a Completer.
    
      Future doStuff() {
        Completer completer = new Completer();
        runDatabaseQuery(sql, (results) {
          completer.complete(results);
        });
        return completer.future;
      }

If you are using an API that already returns a Future, you do not need to use a Completer.

  [1]: https://api.dartlang.org/stable/1.17.1/dart-async/Future-class.html
  [2]: https://api.dartlang.org/stable/1.17.1/dart-async/Stream-class.html

## Returning a Future using a Completer
    Future<Results> costlyQuery() {
      var completer = new Completer();
    
      database.query("SELECT * FROM giant_table", (results) {
        // when complete
        completer.complete(results);
      }, (error) {
        completer.completeException(error);
      });
    
      // this returns essentially immediately,
      // before query is finished
      return completer.future; 
    }



## Async and Await
    import 'dart:async';

    Future main() async {
      var value = await _waitForValue();
      print("Here is the value: $value");
      //since _waitForValue() returns immediately if you un it without await you won't get the result
      var errorValue = "not finished yet";
      _waitForValue();
      print("Here is the error value: $value");// not finished yet
    }

    Future<int> _waitForValue() => new Future((){

      var n = 100000000;

      // Do some long process
      for (var i = 1; i <= n; i++) {
        // Print out progress:
        if ([n / 2, n / 4, n / 10, n / 20].contains(i)) {
          print("Not done yet...");
        }

        // Return value when done.
        if (i == n) {
          print("Done.");
          return i;
        }
      }
    });

See example on Dartpad:  https://dartpad.dartlang.org/11d189b51e0f2680793ab3e16e53613c

