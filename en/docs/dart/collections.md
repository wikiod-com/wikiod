---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Creating a new List
Lists can be created in multiple ways.

The recommended way is to use a `List` literal:

    var vegetables = ['broccoli', 'cabbage'];

The `List` constructor can be used as well:

    var fruits = new List();

If you prefer stronger typing, you can also supply a type parameter in one of the following ways:

    var fruits = <String>['apples', 'oranges'];
    var fruits = new List<String>();

For creating a small growable list, either empty or containing some known initial values, the literal form is preferred. There are specialized constructors for other kinds of lists:

    var fixedLengthList1 = new List(8);
    var fixedLengthList2 = new List.filled(8, "initial text");
    var computedValues = new List.generate(8, (n) => "x" * n);
    var fromIterable = new List<String>.from(computedValues.getRange(2, 5));

See also the [Effective Dart](https://www.dartlang.org/guides/language/effective-dart) style guide about [collections](https://www.dartlang.org/guides/language/effective-dart/usage#collections).

## Creating a new Set
Sets can be created via the constructor:

    var ingredients = new Set();
    ingredients.addAll(['gold', 'titanium', 'xenon']);

## Creating a new Map
Maps can be created in multiple ways.

Using the constructor, you can create a new map as follow:

    var searchTerms = new Map();

Types for the key and value can also be defined using generics:

    var nobleGases = new Map<int, String>();
    var nobleGases = <int, String>{};

Maps can otherwise be created using the map literal:

    var map = {
        "key1": "value1",
        "key2": "value2"
    };



## Map each element in the collection.
All collection objects contain a `map` method that takes a `Function` as an argument, which must take a single argument. This returns an `Iterable` backed by the collection. When the `Iterable` is iterated, each step calls the function with a new element of the collection, and the result of the call becomes the next element of the iteration.

You can turn an `Iterable` into a collection again by using the `Iterable.toSet()` or `Iterable.toList()` methods, or by using a collection constructor which takes an iterable like `Queue.from` or `List.from`.

Example:

    main() {
      var cats = [
        'Abyssinian',
        'Scottish Fold',
        'Domestic Shorthair'
      ];

      print(cats); // [Abyssinian, Scottish Fold, Domestic Shorthair]

      var catsInReverse =
      cats.map((String cat) {
        return new String.fromCharCodes(cat.codeUnits.reversed);
      })
      .toList(); // [nainissybA, dloF hsittocS, riahtrohS citsemoD]

      print(catsInReverse);
    }

See dartpad example here:  https://dartpad.dartlang.org/a18367ff767f172b34ff03c7008a6fa1

## Filter a list
Dart allows to easily filter a list using `where`.

    var fruits = ['apples', 'oranges', 'bananas'];
    fruits.where((f) => f.startsWith('a')).toList(); //apples

Of course you can use some AND or OR operators in your where clause.

