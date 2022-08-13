---
title: "Perfect Forwarding"
slug: "perfect-forwarding"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

Perfect forwarding requires *forwarding references* in order to preserve the ref-qualifiers of the arguments. Such references appear only in a *deduced context*. That is:

    template<class T>
    void f(T&& x) // x is a forwarding reference, because T is deduced from a call to f()
    {
        g(std::forward<T>(x)); // g() will receive an lvalue or an rvalue, depending on x
    }

The following does not involve perfect forwarding, because `T` is not deduced from the constructor call:

    template<class T>
    struct a
    {
        a(T&& x); // x is a rvalue reference, not a forwarding reference
    };


<!-- if version [gte C++17] -->

C++17 will allow deduction of class template arguments. The constructor of "a" in the above example will become a user of a forwarding reference

    a example1(1);
      // same as a<int> example1(1);

    int x = 1;
    a example2(x);
      // same as a<int&> example2(x);

<!-- end version if -->

## Factory functions
Suppose we want to write a factory function that accepts an arbitrary list of arguments and passes those arguments unmodified to another function. An example of such a function is `make_unique`, which is used to safely construct a new instance of `T` and return a `unique_ptr<T>` that owns the instance.

The language rules regarding variadic templates and rvalue references allows us to write such a function.

    template<class T, class... A>
    unique_ptr<T> make_unique(A&&... args)
    {
        return unique_ptr<T>(new T(std::forward<A>(args)...));
    }

The use of ellipses `...` indicate a parameter pack, which represents an arbitrary number of types. The compiler will expand this parameter pack to the correct number of arguments at the call site. These arguments are then passed to `T`'s constructor using `std::forward`. This function is required to preserve the ref-qualifiers of the arguments.

    struct foo
    {
        foo() {}
        foo(const foo&) {}                    // copy constructor
        foo(foo&&) {}                         // copy constructor
        foo(int, int, int) {}
    };

    foo f;
    auto p1 = make_unique<foo>(f);            // calls foo::foo(const foo&)
    auto p2 = make_unique<foo>(std::move(f)); // calls foo::foo(foo&&)
    auto p3 = make_unique<foo>(1, 2, 3); 





