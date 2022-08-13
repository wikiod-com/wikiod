---
title: "Dart-JavaScript interoperability"
slug: "dart-javascript-interoperability"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Dart-JavaScript interoperability lets us run JavaScript code from our Dart programs.

The interoperability is achieved by using the [`js`](https://pub.dartlang.org/packages/js) library to create Dart stubs. These stubs describe the interface we'd like to have with the underlying JavaScript code. At runtime calling the Dart stub will invoke the JavaScript code.

## Wrapping JavaScript classes/namespaces
Suppose we'd like to wrap the Google Maps JavaScript API `google.maps`:

    @JS('google.maps')
    library maps;
    
    import "package:js/js.dart";
    
    @JS()
    class Map {
      external Map(Location location);
      external Location getLocation();
    }

We now have the Map Dart class which corresponds to the JavaScript `google.maps.Map` class.

Running `new Map(someLocation)` in Dart will invoke `new google.maps.Map(location)` in JavaScript.

Note that you don't have to name your Dart class the same as the JavaScript class:

    @JS("LatLng")
    class Location {
      external Location(num lat, num lng);
    }

The `Location` Dart class corresponds to the `google.maps.LatLng` class.

Using inconsistent names is discouraged as they can create confusion.


## Passing object literals
It's common practice in JavaScript to pass object literals to functions:

    // JavaScript
    printOptions({responsive: true});
    Unfortunately we cannot pass Dart Map objects to JavaScript in these cases.

What we have to do is create a Dart object that represents the object literal and contains all of its fields:

    // Dart
    @JS()
    @anonymous
    class Options {
      external bool get responsive;
    
      external factory Options({bool responsive});
    }

Note that the `Options` Dart class doesn't correspond to any JavaScript class. As such we must mark it with the `@anonymous` annotation.

Now we can create a stub for the original printOptions function and call it with a new Options object:

    // Dart
    @JS()
    external printOptions(Options options);
    
    printOptions(new Options(responsive: true));



## Calling a global function
Suppose we'd like to invoke the JavaScript function `JSON.stringify` which receives an object, encodes it into a JSON string and returns it.

All we'd have to do is write the function signature, mark it as external and annotate it with the `@JS` annotation:

     @JS("JSON.stringify")
    external String stringify(obj);

The `@JS` annotation will be used from here on out to mark Dart classes that we'd like to use in JavaScript as well.

