---
title: "Libraries"
slug: "libraries"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The `import` and `library` directives can help you create a modular and shareable code base. Every Dart app is a `library`, even if it doesn’t use a library directive. Libraries can be distributed using packages. See [Pub Package and Asset Manager][1] for information about pub, a package manager included in the SDK.


  [1]: https://www.dartlang.org/tools/pub

## Using libraries
Use `import` to specify how a namespace from one library is used in the scope of another library.

    import 'dart:html';

The only required argument to `import` is a URI specifying the library. For built-in libraries, the URI has the special `dart:` scheme. For other libraries, you can use a file system path or the `package:` scheme. The `package:` scheme specifies libraries provided by a package manager such as the pub tool. For example:

    import 'dart:io';
    import 'package:mylib/mylib.dart';
    import 'package:utils/utils.dart';



## Libraries and visibility
Unlike Java, Dart doesn’t have the keywords `public`, `protected`, and private. If an identifier starts with an underscore `_`, it’s private to its library.

If you for example have class A in a separate library file (eg, `other.dart`), such as:

    library other;
    
    class A {
      int _private = 0;
    
      testA() {
        print('int value: $_private');  // 0
        _private = 5;
        print('int value: $_private'); // 5
      }
    }

and then import it into your main app, such as:

    import 'other.dart';
    
    void main() {
      var b = new B();
      b.testB();    
    }
    
    class B extends A {
      String _private;
    
      testB() {
        _private = 'Hello';
        print('String value: $_private'); // Hello
        testA();
        print('String value: $_private'); // Hello
      }
    }

You get the expected output:

    String value: Hello
    int value: 0
    int value: 5
    String value: Hello



## Specifying a library prefix
If you import two libraries that have conflicting identifiers, then you can specify a prefix for one or both libraries. For example, if library1 and library2 both have an Element class, then you might have code like this:

    import 'package:lib1/lib1.dart';
    import 'package:lib2/lib2.dart' as lib2;
    // ...
    var element1 = new Element(); // Uses Element from lib1.
    var element2 =
        new lib2.Element();       // Uses Element from lib2.

## Importing only part of a library
If you want to use only part of a library, you can selectively import the library. For example:

    // Import only foo and bar.
    import 'package:lib1/lib1.dart' show foo, bar;
    
    // Import all names EXCEPT foo.
    import 'package:lib2/lib2.dart' hide foo;

## Lazily loading a library
Deferred loading (also called lazy loading) allows an application to load a library on demand, if and when it’s needed. To lazily load a library, you must first import it using deferred as.

    import 'package:deferred/hello.dart' deferred as hello;

When you need the library, invoke loadLibrary() using the library’s identifier.
    
    greet() async {
      await hello.loadLibrary();
      hello.printGreeting();
    }

In the preceding code, the `await` keyword pauses execution until the library is loaded. For more information about `async` and `await`, see more examples here [asynchrony support][1] or visit the [asynchrony support][2] part of the language tour.

  [1]: https://www.wikiod.com/dart/asynchronous-programming
  [2]: https://www.dartlang.org/guides/language/language-tour#asynchrony

