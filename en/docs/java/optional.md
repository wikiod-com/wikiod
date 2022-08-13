---
title: "Optional"
slug: "optional"
draft: false
images: []
weight: 9195
type: docs
toc: true
---

`Optional` is a container object which may or may not contain a non-null value. If a value is present, `isPresent()` will return `true` and `get()` will return the value.

Additional methods that depend on the presence of the contained value are provided, such as [`orElse()`][1], which returns a default value if value not present, and `ifPresent()` which executes a block of code if the value is present.

  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#orElse-T-

## Syntax
- Optional.empty() // Creates an empty Optional instance.
- Optional.of(value) // Returns an Optional with the specified non-null value. A NullPointerException will be thrown if the passed value is null.
- Optional.ofNullable(value) // Returns an Optional with the specified value that may be null.

## Map
Use the [`map()`][1] method of `Optional` to work with values that might be `null` without doing explicit `null` checks:

(Note that the [`map()`][1] and [`filter()`][2] operations are evaluated immediately, unlike their Stream counterparts which are only evaluated upon a *terminal operation*.)

Syntax:

    public <U> Optional<U> map(Function<? super T,? extends U> mapper)

 Code examples:

    String value = null;

    return Optional.ofNullable(value).map(String::toUpperCase).orElse("NONE");
    // returns "NONE"

<!-- -->

    String value = "something";

    return Optional.ofNullable(value).map(String::toUpperCase).orElse("NONE");
    // returns "SOMETHING"


Because [Optional.map()][1] returns an empty optional when its mapping function returns null, you can chain several map() operations as a form of null-safe dereferencing. This is also known as **Null-safe chaining**.

Consider the following example:

    String value = foo.getBar().getBaz().toString();

Any of `getBar`, `getBaz`, and `toString` can potentially throw a `NullPointerException`.

Here is an alternative way to get the value from `toString()` using `Optional`:

    String value = Optional.ofNullable(foo)
                           .map(Foo::getBar)
                           .map(Bar::getBaz)
                           .map(Baz::toString)
                           .orElse("");

This will return an empty string if any of the mapping functions returned null.


Below is an another example, but slightly different. It will print the value only if none of the mapping functions returned null.

    Optional.ofNullable(foo)
            .map(Foo::getBar)
            .map(Bar::getBaz)
            .map(Baz::toString)
            .ifPresent(System.out::println);


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#map-java.util.function.Function-
  [2]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#filter-java.util.function.Predicate-

## Return default value if Optional is empty
Don't just use [`Optional.get()`][1] since that may throw `NoSuchElementException`. 
The [`Optional.orElse(T)`][2] and [`Optional.orElseGet(Supplier<? extends T>)`][3] methods provide a way to supply a default value in case the Optional is empty.

    String value = "something";

    return Optional.ofNullable(value).orElse("defaultValue");
    // returns "something"

    return Optional.ofNullable(value).orElseGet(() -> getDefaultValue());
    // returns "something" (never calls the getDefaultValue() method)

<!-- -->
   
    String value = null;

    return Optional.ofNullable(value).orElse("defaultValue");
    // returns "defaultValue"

    return Optional.ofNullable(value).orElseGet(() -> getDefaultValue());
    // calls getDefaultValue() and returns its results

The crucial difference between the `orElse` and `orElseGet` is that the latter is only evaluated when the Optional is empty while the argument supplied to the former one is evaluated even if the Optional is not empty. The `orElse` should therefore only be used for constants and never for supplying value based on any sort of computation.   


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#get--
  [2]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#orElse-T-
  [3]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#orElseGet-java.util.function.Supplier-

## Throw an exception, if there is no value
Use the [`orElseThrow()`][1] method of `Optional` to get the contained value or throw an exception, if it hasn't been set. This is similar to calling `get()`, except that it allows for arbitrary exception types. The method takes a supplier that must return the exception to be thrown.

In the first example, the method simply returns the contained value:

    Optional optional = Optional.of("something");

    return optional.orElseThrow(IllegalArgumentException::new);
    // returns "something" string

In the second example, the method throws an exception because a value hasn't been set:

    Optional optional = Optional.empty();

    return optional.orElseThrow(IllegalArgumentException::new);
    // throws IllegalArgumentException

You can also use the lambda syntax if throwing an exception with message is needed:

    optional.orElseThrow(() -> new IllegalArgumentException("Illegal"));

  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#orElseThrow-java.util.function.Supplier-

## Lazily provide a default value using a Supplier
The *normal* [`orElse`][1] method takes an `Object`, so you might wonder why there is an option to provide a `Supplier` here (the `orElseGet` method).

Consider:

    String value = "something";
    return Optional.ofNullable(value)
                   .orElse(getValueThatIsHardToCalculate()); // returns "something"

It would still call `getValueThatIsHardToCalculate()` even though it's result is not used as the optional is not empty.

To avoid this penalty you supply a supplier:

    String value = "something";
    return Optional.ofNullable(value)
                   .orElseGet(() -> getValueThatIsHardToCalculate()); // returns "something"

This way `getValueThatIsHardToCalculate()` will only be called if the `Optional` is empty.


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#orElse-T-

## Filter
[`filter()`][1] is used to indicate that you would like the value _only if_ it matches your predicate. 

Think of it like `if (!somePredicate(x)) { x = null; }`.

Code examples:

    String value = null;
    Optional.ofNullable(value) // nothing
            .filter(x -> x.equals("cool string"))// this is never run since value is null
            .isPresent(); // false
<!---->

    String value = "cool string";
    Optional.ofNullable(value) // something
            .filter(x -> x.equals("cool string"))// this is run and passes
            .isPresent(); // true

<!---->

    String value = "hot string";
    Optional.ofNullable(value) // something
            .filter(x -> x.equals("cool string"))// this is run and fails
            .isPresent(); // false


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#filter-java.util.function.Predicate-

## Using Optional containers for primitive number types
[`OptionalDouble`][1], [`OptionalInt`][2] and [`OptionalLong`][3] work like `Optional`, but are specifically designed to wrap primitive types:

    OptionalInt presentInt = OptionalInt.of(value);
    OptionalInt absentInt = OptionalInt.empty();

Because numeric types do have a value, there is no special handling for null. Empty containers can be checked with:

    presentInt.isPresent(); // Is true.
    absentInt.isPresent(); // Is false.

Similarly, shorthands exist to aid value management:

    // Prints the value since it is provided on creation.
    presentInt.ifPresent(System.out::println);

    // Gives the other value as the original Optional is empty.
    int finalValue = absentInt.orElseGet(this::otherValue);

    // Will throw a NoSuchElementException.
    int nonexistentValue = absentInt.getAsInt();


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/OptionalDouble.html
  [2]: https://docs.oracle.com/javase/8/docs/api/java/util/OptionalInt.html
  [3]: https://docs.oracle.com/javase/8/docs/api/java/util/OptionalLong.html

## Run code only if there is a value present
    Optional<String> optionalWithValue = Optional.of("foo");
    optionalWithValue.ifPresent(System.out::println);//Prints "foo".

    Optional<String> emptyOptional = Optional.empty();
    emptyOptional.ifPresent(System.out::println);//Does nothing.

## FlatMap
[`flatMap`][1] is similar to [`map`][2].  The difference is described by the javadoc as follows:

> This method is similar to `map(Function)`, but the provided mapper is one whose result is already an `Optional`, and if invoked, `flatMap` does not wrap it with an additional `Optional`.

In other words, when you chain a method call that returns an `Optional`, using `Optional.flatMap` avoids creating nested `Optionals`.

For example, given the following classes: 

    public class Foo {
        Optional<Bar> getBar(){
            return Optional.of(new Bar());
        }
    }
    
    public class Bar {
    }

If you use `Optional.map`, you will get a nested `Optional`; i.e. `Optional<Optional<Bar>>`.

    Optional<Optional<Bar>> nestedOptionalBar =
        Optional.of(new Foo())
            .map(Foo::getBar);

However, if you use `Optional.flatMap`, you will get a simple `Optional`; i.e. `Optional<Bar>`.

    Optional<Bar> optionalBar =
        Optional.of(new Foo())
            .flatMap(Foo::getBar);


  [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#flatMap-java.util.function.Function-
  [2]: https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html#map-java.util.function.Function-

