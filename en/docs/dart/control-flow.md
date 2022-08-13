---
title: "Control Flow"
slug: "control-flow"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## If Else
Dart has If Else:

    if (year >= 2001) {
      print('21st century');
    } else if (year >= 1901) {
      print('20th century');
    } else {
      print('We Must Go Back!');  
    }

Dart also has a ternary `if` operator:

    var foo = true;
    print(foo ? 'Foo' : 'Bar'); // Displays "Foo".

## While Loop
While loops and do while loops are allowed in Dart:

    while(peopleAreClapping()) {
      playSongs();
    }

and:

    do {
      processRequest();
    } while(stillRunning());

Loops can be terminated using a break:

    while (true) {
      if (shutDownRequested()) break;
      processIncomingRequests();
    }

You can skip iterations in a loop using continue:

    for (var i = 0; i < bigNumber; i++) {
      if (i.isEven){
        continue;
      }
      doSomething();
    }



## For Loop
Two types of for loops are allowed:

    for (int month = 1; month <= 12; month++) {
      print(month);
    }

and:

    for (var object in flybyObjects) {
      print(object);
    }

The `for-in` loop is convenient when simply iterating over an `Iterable` collection. There is also a [`forEach`][1] method that you can call on `Iterable` objects that behaves like `for-in`:

    flybyObjects.forEach((object) => print(object));

or, more concisely:

    flybyObjects.forEach(print);


  [1]: https://api.dartlang.org/stable/1.17.1/dart-core/Iterable/forEach.html

## Switch Case
Dart has a switch case which can be used instead of long if-else statements:

    var command = 'OPEN';
    
    switch (command) {
      case 'CLOSED':
        executeClosed();
        break;
      case 'OPEN':
        executeOpen();
        break;
      case 'APPROVED':
        executeApproved();
        break;
      case 'UNSURE':
        // missing break statement means this case will fall through
        // to the next statement, in this case the default case
      default:
        executeUnknown();
    }

You can only compare integer, string, or compile-time constants. The compared objects must be instances of the same class (and not of any of its subtypes), and the class must not override ==.

One surprising aspect of switch in Dart is that non-empty case clauses must end with break, or less commonly, continue, throw, or return. That is, non-empty case clauses cannot fall through. You must explicitly end a non-empty case clause, usually with a break. You will get a static warning if you omit break, continue, throw, or return, and the code will error at that location at runtime.

    var command = 'OPEN';
    switch (command) {
      case 'OPEN':
        executeOpen();
        // ERROR: Missing break causes an exception to be thrown!!

      case 'CLOSED': // Empty case falls through
      case 'LOCKED':
        executeClosed();
        break;
    }

If you want fall-through in a non-empty `case`, you can use `continue` and a label:

          var command = 'OPEN';
          switch (command) {
            case 'OPEN':
              executeOpen();
              continue locked;
    locked: case 'LOCKED':
              executeClosed();
              break;
          }

