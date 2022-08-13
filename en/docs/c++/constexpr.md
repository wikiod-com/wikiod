---
title: "constexpr"
slug: "constexpr"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

`constexpr` is a [keyword](https://www.wikiod.com/docs/c%2b%2b/4891/keywords) that can be used to mark a variable's value as a constant expression, a function as potentially usable in constant expressions, or (since C++17) an [if statement](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18547/if) as having only one of its branches selected to be compiled.

The `constexpr` keyword was added in C++11 but for a few years since the C++11 standard was published, not all major compilers supported it.  at the time that the C++11 standard was published. As of the time of publication of C++14, all major compilers support `constexpr`.

## constexpr variables
A variable declared `constexpr` is implicitly `const` and its value may be used as a constant expression.

**Comparison with `#define`**

A `constexpr` is type-safe replacement for `#define` based compile-time expressions. With `constexpr` the compile-time evaluated expression is replaced with the result. For example:

<!-- if version [gte C++11] -->
    int main()
    {
       constexpr int N = 10 + 2;
       cout << N;
    }
<!-- end version if -->

will produce the following code:

    cout << 12;

A pre-processor based compile-time macro would be different. Consider:

    #define N 10 + 2

    int main()
    {
        cout << N;
    }

will produce:

    cout << 10 + 2;

which will obviously be converted to `cout << 10 + 2;`. However, the compiler would have to do more work. Also, it creates a problem if not used correctly. 

For example (with `#define`):

    cout << N * 2;

forms:

    cout << 10 + 2 * 2; // 14

But a pre-evaluated `constexpr` would correctly give `24`. 

**Comparison with `const`**

A `const` variable is a **variable** which needs memory for its storage. A `constexpr` does not. A `constexpr` produces compile time constant, which cannot be changed. You may argue that `const` may also not be changed. But consider:

    int main()
    {
       const int size1 = 10;
       const int size2 = abs(10);
    
       int arr_one[size1]; 
       int arr_two[size2]; 
    }

With most compilers the second statement will fail (may work with GCC, for example). The size of any array, as you might know, has to be a constant expression (i.e. results in compile-time value). The second variable `size2` is assigned some value that is decided at runtime (even though you know it is `10`, for the compiler it is not compile-time). 

This means that a `const` may or may not be a true compile-time constant. You cannot guarantee or enforce that a particular `const` value is absolutely compile-time. You may use `#define` but it has its own pitfalls.

Therefore simply use:

<!-- if version [gte C++11] -->
    int main()
    {
        constexpr int size = 10;
    
        int arr[size];
    }
<!-- end version if -->

A `constexpr` expression must evaluate to a compile-time value. Thus, you cannot use:

<!-- if version [gte C++11] -->
    constexpr int size = abs(10);
<!-- end version if -->

Unless the function (`abs`) is itself returning a `constexpr`.

All basic types can be initialized with `constexpr`.

<!-- if version [gte C++11] -->
    constexpr bool FailFatal = true;
    constexpr float PI = 3.14f;
    constexpr char* site= "StackOverflow";
<!-- end version if -->

Interestingly, and conveniently, you may also use `auto`:

<!-- if version [gte C++11] -->
    constexpr auto domain = ".COM";  // const char * const domain = ".COM"
    constexpr auto PI = 3.14;        // constexpr double
<!-- end version if -->


## Static if statement
<!-- if version [gte C++17] -->
The `if constexpr` statement can be used to conditionally compile code. The condition must be a constant expression. The branch not selected is *discarded.* A discarded statement inside a template is not instantiated. For example:

    template<class T, class ... Rest>
    void g(T &&p, Rest &&...rs)
    {
      // ... handle p
      if constexpr (sizeof...(rs) > 0)
        g(rs...);  // never instantiated with an empty argument list
    }

In addition, variables and functions that are odr-used only inside discarded statements are not required to be defined, and discarded `return` statements are not used for function return type deduction.

`if constexpr` is distinct from `#ifdef`. `#ifdef` conditionally compiles code, but only based on conditions that can be evaluated at preprocessing time. For example, `#ifdef` could not be used to conditionally compile code depending on the value of a template parameter. On the other hand, `if constexpr` cannot be used to discard syntactically invalid code, while `#ifdef` can.

    if constexpr(false) {
        foobar;  // error; foobar has not been declared
        std::vector<int> v("hello, world");  // error; no matching constructor
    }

<!-- end version if -->

## constexpr functions
A function that is declared `constexpr` is implicitly inline and calls to such a function potentially yield constant expressions. For example, the following function, if called with constant expression arguments, yields a constant expression too:

<!-- if version [gte C++11] -->
    constexpr int Sum(int a, int b)
    {
        return a + b;
    }
<!-- end version if -->

Thus, the result of the function call may be used as an array bound or a template argument, or to initialize a `constexpr` variable:

<!-- if version [gte C++11] -->
    int main()
    {
        constexpr int S = Sum(10,20);
       
        int Array[S];
        int Array2[Sum(20,30)]; // 50 array size, compile time
    }
<!-- end version if -->

Note that if you remove `constexpr` from function's return type specification, assignment to `S` will not work, as `S` is a `constexpr` variable, and must be assigned a compile-time const. Similarly, size of array will also not be a constant-expression, if function `Sum` is not `constexpr`.

Interesting thing about `constexpr` functions is that you may also use it like ordinary functions:

<!-- if version [gte C++11] -->
    int a = 20;
    auto sum = Sum(a, abs(-20));
<!-- end version if -->

`Sum` will not be a `constexpr` function now, it will be compiled as an ordinary function, taking variable (non-constant) arguments, and returning non-constant value. You need not to write two functions.

It also means that if you try to assign such call to a non-const variable, it won't compile:

<!-- if version [gte C++11] -->
    int a = 20;
    constexpr auto sum = Sum(a, abs(-20));
<!-- end version if -->

The reason is simple: `constexpr` must only be assigned a compile-time constant. However, the above function call makes `Sum` a non-`constexpr` (R-value is non-const, but L-value is declaring itself to be `constexpr`).

----------
The `constexpr` function **must** also return a compile-time constant.  Following will not compile:

<!-- if version [gte C++11] -->
    constexpr int Sum(int a, int b)
    {
        int a1 = a;     // ERROR
        return a + b;
    }
<!-- end version if -->

Because `a1` is a non-constexpr *variable*, and prohibits the function from being a true `constexpr` function. Making it `constexpr` and assigning it `a` will also not work - since value of `a` (incoming parameter) is still not yet known:

<!-- if version [gte C++11] -->
    constexpr int Sum(int a, int b)
    {
       constexpr int a1 = a;     // ERROR
       ..
<!-- end version if -->

Furthermore, following will also not compile:

<!-- if version [gte C++11] -->
    constexpr int Sum(int a, int b)
    {
       return abs(a) + b; // or abs(a) + abs(b)
    }
<!-- end version if -->

Since `abs(a)` is not a constant expression (even `abs(10)` will not work, since `abs` is not returning a `constexpr int` !

What about this?

<!-- if version [gte C++11] -->
    constexpr int Abs(int v)
    {
        return v >= 0 ? v : -v;
    }
    
    constexpr int Sum(int a, int b)
    {
        return Abs(a) + b;
    }
<!-- end version if -->

We crafted our own `Abs` function which is a `constexpr`, and the body of `Abs` also doesn't break any rule. Also, at the call site (inside `Sum`), the expression evaluates to a `constexpr`. Hence, the call to `Sum(-10, 20)` will be a compile-time constant expression resulting to `30`.

