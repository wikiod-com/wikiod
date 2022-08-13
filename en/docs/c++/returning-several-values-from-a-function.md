---
title: "Returning several values from a function"
slug: "returning-several-values-from-a-function"
draft: false
images: []
weight: 7814
type: docs
toc: true
---

There are many situations where it is useful to return several values from a function: for example, if you want to input an item and return the price and number in stock, this functionality could be useful. There are many ways to do this in C++, and most involve the STL. However, if you wish to avoid the STL for some reason, there are still several ways to do this, including `structs/classes` and `arrays`. 

## Using std::tuple
<!-- if version [gte C++11] -->

The type [`std::tuple`][1] can bundle any number of values, potentially including values of different types, into a single return object:

    std::tuple<int, int, int, int> foo(int a, int b) { // or auto (C++14)
       return std::make_tuple(a + b, a - b, a * b, a / b);
    }

In C++17, a braced initializer list can be used:

<!-- if version [gte C++17] -->
    std::tuple<int, int, int, int> foo(int a, int b)    {
        return {a + b, a - b, a * b, a / b};
    }
<!-- end version if -->

Retrieving values from the returned `tuple` can be cumbersome, requiring the use of the [`std::get`][3] template function:

    auto mrvs = foo(5, 12);
    auto add = std::get<0>(mrvs);
    auto sub = std::get<1>(mrvs);
    auto mul = std::get<2>(mrvs);
    auto div = std::get<3>(mrvs);

If the types can be declared before the function returns, then [`std::tie`][4] can be employed to unpack a `tuple` into existing variables:

    int add, sub, mul, div;
    std::tie(add, sub, mul, div) = foo(5, 12);

If one of the returned values is not needed, [`std::ignore`][5] can be used:

    std::tie(add, sub, std::ignore, div) = foo(5, 12);

<!-- if version [gte C++17] -->
[Structured bindings][2] can be used to avoid `std::tie`:

    auto [add, sub, mul, div] = foo(5,12);
<!-- end version if -->

If you want to return a tuple of lvalue references instead of a tuple of values, use `std::tie` in place of [`std::make_tuple`][6].

    std::tuple<int&, int&> minmax( int& a, int& b ) {
      if (b<a)
        return std::tie(b,a);
      else
        return std::tie(a,b);
    }

which permits

    void increase_least(int& a, int& b) {
      std::get<0>(minmax(a,b))++;
    }

In some rare cases you'll use [`std::forward_as_tuple`][7] instead of `std::tie`; be careful if you do so, as temporaries may not last long enough to be consumed.

<!-- end version if -->

  [1]: http://en.cppreference.com/w/cpp/utility/tuple
  [2]: https://www.wikiod.com/docs/c%2b%2b/487/functions-with-multiple-return-values/3384/structured-bindings#t=201607230711000500592
  [3]: http://en.cppreference.com/w/cpp/utility/tuple/get
  [4]: http://en.cppreference.com/w/cpp/utility/tuple/tie
  [5]: http://en.cppreference.com/w/cpp/utility/tuple/ignore
  [6]: http://en.cppreference.com/w/cpp/utility/tuple/make_tuple
  [7]: http://en.cppreference.com/w/cpp/utility/tuple/forward_as_tuple


## Structured Bindings
<!-- if version [gte C++17] -->
C++17 introduces structured bindings, which makes it even easier to deal with multiple return types, as you do not need to rely upon [`std::tie()`][1] or do any manual tuple unpacking:

    std::map<std::string, int> m;

    // insert an element into the map and check if insertion succeeded
    auto [iterator, success] = m.insert({"Hello", 42});

    if (success) {
        // your code goes here
    }

    // iterate over all elements without having to use the cryptic 'first' and 'second' names
    for (auto const& [key, value] : m) {
        std::cout << "The value for " << key << " is " << value << '\n';
    }

Structured bindings can be used by default with `std::pair`, `std::tuple`, and any type whose non-static data members are all either public direct members or members of an unambiguous base class:

    struct A { int x; };
    struct B : A { int y; };
    B foo();

    // with structured bindings
    const auto [x, y] = foo();

    // equivalent code without structured bindings
    const auto result = foo();
    auto& x = result.x;
    auto& y = result.y;

If you make your type "tuple-like" it will also automatically work with your type. A tuple-like is a type with appropriate `tuple_size`, `tuple_element` and `get` written:

    namespace my_ns {
        struct my_type {
            int x;
            double d;
            std::string s;
        };
        struct my_type_view {
            my_type* ptr;
        };
    }

    namespace std {
        template<>
        struct tuple_size<my_ns::my_type_view> : std::integral_constant<std::size_t, 3>
        {};

        template<> struct tuple_element<my_ns::my_type_view, 0>{ using type = int; };
        template<> struct tuple_element<my_ns::my_type_view, 1>{ using type = double; };
        template<> struct tuple_element<my_ns::my_type_view, 2>{ using type = std::string; };
    }

    namespace my_ns {
        template<std::size_t I>
        decltype(auto) get(my_type_view const& v) {
            if constexpr (I == 0)
                return v.ptr->x;
            else if constexpr (I == 1)
                return v.ptr->d;
            else if constexpr (I == 2)
                return v.ptr->s;
            static_assert(I < 3, "Only 3 elements");
        }
    }

now this works:

    my_ns::my_type t{1, 3.14, "hello world"};

    my_ns::my_type_view foo() {
        return {&t};
    }

    int main() {
        auto[x, d, s] = foo();
        std::cout << x << ',' << d << ',' << s << '\n';
    }

<!-- end version if -->

  [1]: http://en.cppreference.com/w/cpp/utility/tuple/tie

## Using struct
A [`struct`][1] can be used to bundle multiple return values:

<!-- if version [gte C++11] -->
    struct foo_return_type {
        int add;
        int sub;
        int mul;
        int div;
    };

    foo_return_type foo(int a, int b) {
        return {a + b, a - b, a * b, a / b};
    }
    
    auto calc = foo(5, 12);
<!-- end version if -->

<!-- if version [lt C++11] -->
Instead of assignment to individual fields, a constructor can be used to simplify the constructing of returned values:

    struct foo_return_type {
        int add;
        int sub;
        int mul;
        int div;
        foo_return_type(int add, int sub, int mul, int div)
        : add(add), sub(sub), mul(mul), div(div) {}
    };
    
    foo_return_type foo(int a, int b) {
         return foo_return_type(a + b, a - b, a * b, a / b);
    }
    
    foo_return_type calc = foo(5, 12);
<!-- end version if -->
    
The individual results returned by the function `foo()` can be retrieved by accessing the member variables of the `struct` `calc`:

    std::cout << calc.add << ' ' << calc.sub << ' ' << calc.mul << ' ' << calc.div << '\n';
       
**Output:**

> 17 -7 60 0

Note: When using a `struct`, the returned values are grouped together in a single object and accessible using meaningful names. This also helps to reduce the number of extraneous variables created in the scope of the returned values.

<!-- if version [gte C++17] -->
In order to unpack a `struct` returned from a function, [structured bindings][2] can be used. This places the out-parameters on an even footing with the in-parameters:

    int a=5, b=12;
    auto[add, sub, mul, div] = foo(a, b);
    std::cout << add << ' ' << sub << ' ' << mul << ' ' << div << '\n';
The output of this code is identical to that above. The `struct` is still used to return the values from the function. This permits you do deal with the fields individually.
<!-- end version if -->

  [1]: http://en.cppreference.com/w/cpp/language/class
  [2]: https://www.wikiod.com/docs/c%2b%2b/487/functions-with-multiple-return-values/3384/structured-bindings

## Using Output Parameters
Parameters can be used for returning one or more values; those parameters are required to be non-`const` pointers or references.

References:

    void calculate(int a, int b, int& c, int& d, int& e, int& f) {
        c = a + b;
        d = a - b;
        e = a * b;
        f = a / b;
    }

Pointers:

    void calculate(int a, int b, int* c, int* d, int* e, int* f) {
        *c = a + b;
        *d = a - b;
        *e = a * b;
        *f = a / b;
    }

Some libraries or frameworks use an empty 'OUT' `#define` to make it abundantly obvious which parameters are output parameters in the function signature. This has no functional impact, and will be compiled out, but makes the function signature a bit clearer;

    #define OUT

    void calculate(int a, int b, OUT int& c) {
        c = a + b;
    }


## Using a Function Object Consumer
We can provide a consumer that will be called with the multiple relevant values:

<!-- if version [gte C++11] -->
    template <class F>
    void foo(int a, int b, F consumer) {
        consumer(a + b, a - b, a * b, a / b);
    }

    // use is simple... ignoring some results is possible as well
    foo(5, 12, [](int sum, int , int , int ){
        std::cout << "sum is " << sum << '\n';
    });
This is known as ["continuation passing style"](https://en.wikipedia.org/wiki/Continuation-passing_style).

You can adapt a function returning a tuple into a continuation passing style function via:
<!-- if version [gte C++17] -->
    template<class Tuple>
    struct continuation {
      Tuple t;
      template<class F>
      decltype(auto) operator->*(F&& f)&&{
        return std::apply( std::forward<F>(f), std::move(t) );
      }
    };
    std::tuple<int,int,int,int> foo(int a, int b);

    continuation(foo(5,12))->*[](int sum, auto&&...) {
      std::cout << "sum is " << sum << '\n';
    };
<!-- end version if -->
with more complex versions being writable in C++14 or C++11.
<!-- end version if -->


## Using std::pair
The struct template [`std::pair`][1] can bundle together *exactly* two return values, of any two types:

    #include <utility>
    std::pair<int, int> foo(int a, int b) {
        return std::make_pair(a+b, a-b);
    }

With C++11 or later, an initializer list can be used instead of `std::make_pair`:

<!-- if version [gte C++11] -->
    #include <utility>
    std::pair<int, int> foo(int a, int b) {
        return {a+b, a-b};
    }
<!-- end version if -->


The individual values of the returned `std::pair` can be retrieved by using the pair's `first` and `second` member objects:

    std::pair<int, int> mrvs = foo(5, 12);
    std::cout << mrvs.first + mrvs.second << std::endl;

Output:

> 10

  [1]: http://en.cppreference.com/w/cpp/utility/pair

## Using std::array
<!-- if version [gte C++11] -->

The container `std::array` can bundle together a fixed number of return values. This number has to be known at compile-time and all return values have to be of the same type:

    std::array<int, 4> bar(int a, int b) {
        return { a + b, a - b, a * b, a / b };
    }

This replaces c style arrays of the form `int bar[4]`. The advantage being that various `c++` std functions can now be used on it. It also provides useful member functions like `at` which is a safe member access function with bound checking, and `size` which allows you to return the size of the array without calculation.

<!-- end version if -->

## Using Output Iterator
Several values of the same type can be returned by passing an output iterator to the function. This is particularly common for generic functions (like the algorithms of the standard library).

Example:

    template<typename Incrementable, typename OutputIterator>
    void generate_sequence(Incrementable from, Incrementable to, OutputIterator output) {
        for (Incrementable k = from; k != to; ++k)
            *output++ = k;
    }

Example usage:

    std::vector<int> digits;
    generate_sequence(0, 10, std::back_inserter(digits));
    // digits now contains {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}


## Using std::vector
A `std::vector` can be useful for returning a dynamic number of variables of the same type. The following example uses `int` as data type, but a `std::vector` can hold any type that is trivially copyable:

    #include <vector>
    #include <iostream>
    
    // the following function returns all integers between and including 'a' and 'b' in a vector
    // (the function can return up to std::vector::max_size elements with the vector, given that
    // the system's main memory can hold that many items)
    std::vector<int> fillVectorFrom(int a, int b) {
        std::vector<int> temp;
        for (int i = a; i <= b; i++) {
            temp.push_back(i);
        }
        return temp;
    }

    int main() {    
        // assigns the filled vector created inside the function to the new vector 'v'
        std::vector<int> v = fillVectorFrom(1, 10);
    
        // prints "1 2 3 4 5 6 7 8 9 10 "
        for (int i = 0; i < v.size(); i++) {
            std::cout << v[i] << " ";
        }
        std::cout << std::endl;
        return 0;
    }

