---
title: "Type Keywords"
slug: "type-keywords"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## class
1. Introduces the definition of a [class](https://www.wikiod.com/docs/c%2b%2b/508/classes-structures) type.

       class foo {
           int x;
         public:
           int get_x();
           void set_x(int new_x);
       };

2. Introduces an *elaborated type specifier,* which specifies that the following name is the name of a class type. If the class name has been declared already, it can be found even if hidden by another name. If the class name has not been declared already, it is forward-declared.

       class foo; // elaborated type specifier -> forward declaration
       class bar {
         public:
           bar(foo& f);
       };
       void baz();
       class baz; // another elaborated type specifer; another forward declaration
                  // note: the class has the same name as the function void baz()
       class foo {
           bar b;
           friend class baz; // elaborated type specifier refers to the class,
                             // not the function of the same name
         public:
           foo();
       };

3. Introduces a type parameter in the declaration of a [template](https://www.wikiod.com/docs/c%2b%2b/460/templates).

       template <class T>
       const T& min(const T& x, const T& y) {
           return b < a ? b : a;
       }

4. In the declaration of a [template template parameter](https://www.wikiod.com/docs/c%2b%2b/460/templates/10838/template-template-parameters), the keyword `class` precedes the name of the parameter. Since the argument for a template template parameter can only be a class template, the use of `class` here is redundant. However, the grammar of C++ requires it.

       template <template <class T> class U>
       //                           ^^^^^ "class" used in this sense here;
       //                                 U is a template template parameter
       void f() {
           U<int>::do_it();
           U<double>::do_it();
       }
5. Note that sense 2 and sense 3 may be combined in the same declaration. For example:

       template <class T>
       class foo {
       };

       foo<class bar> x; // <- bar does not have to have previously appeared.

<!-- if version [gte C++11] -->
6. In the declaration or definition of an enum, declares the enum to be a [scoped enum](https://www.wikiod.com/docs/c%2b%2b/2796/enumeration/13318/scoped-enums#t=201608050832023792595).

       enum class Format {
           TEXT,
           PDF,
           OTHER,
       };
       Format f = F::TEXT;
<!-- end version if -->

## enum
1. Introduces the definition of an [enumeration type](https://www.wikiod.com/docs/c%2b%2b/2796/enumeration).

       enum Direction {
           UP,
           LEFT,
           DOWN,
           RIGHT
       };
       Direction d = UP;

<!-- if version [gte C++11] -->

   In C++11, `enum` may optionally be followed by `class` or `struct` to define a [scoped enum](https://www.wikiod.com/docs/c%2b%2b/2796/enumeration/13318/scoped-enums). Furthermore, both scoped and unscoped enums can have their underlying type explicitly specified by `: T` following the enum name, where `T` refers to an integer type.

       enum class Format : char {
           TEXT,
           PDF,
           OTHER
       };
       Format f = Format::TEXT;

       enum Language : int {
           ENGLISH,
           FRENCH,
           OTHER
       };

Enumerators in normal `enum`s may also be preceded by the scope operator, although they are still considered to be in the scope the `enum` was defined in.

       Language l1, l2;

       l1 = ENGLISH;
       l2 = Language::OTHER;

<!-- end version if -->

2. Introduces an *elaborated type specifier,* which specifies that the following name is the name of a previously declared enum type. (An elaborated type specifier cannot be used to forward-declare an enum type.) An enum can be named in this way even if hidden by another name.

       enum Foo { FOO };
       void Foo() {}
       Foo foo = FOO;      // ill-formed; Foo refers to the function
       enum Foo foo = FOO; // ok; Foo refers to the enum type

<!-- if version [gte C++11] -->
3. Introduces an *opaque enum declaration,* which declares an enum without defining it. It can either redeclare a previously declared enum, or forward-declare an enum that has not been previously declared.

   An enum first declared as scoped cannot later be declared as unscoped, or *vice versa.* All declarations of an enum must agree in underlying type.

   When forward-declaring an unscoped enum, the underlying type must be explicitly specified, since it cannot be inferred until the values of the enumerators are known.

       enum class Format; // underlying type is implicitly int
       void f(Format f);
       enum class Format {
           TEXT,
           PDF,
           OTHER,
       };

       enum Direction;    // ill-formed; must specify underlying type

<!-- end version if -->

## struct
Interchangeable with [`class`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18504/class), except for the following differences:

* If a class type is defined using the keyword `struct`, then the default accessibility of bases and members is `public` rather than `private`.
* `struct` cannot be used to declare a template type parameter or template template parameter; only `class` can.

## union
1. Introduces the definition of a [union](https://www.wikiod.com/docs/c%2b%2b/2678/unions) type.

       // Example is from POSIX
       union sigval {
           int     sival_int;
           void   *sival_ptr;
       };

2. Introduces an *elaborated type specifier,* which specifies that the following name is the name of a union type. If the union name has been declared already, it can be found even if hidden by another name. If the union name has not been declared already, it is forward-declared.

       union foo; // elaborated type specifier -> forward declaration
       class bar {
         public:
           bar(foo& f);
       };
       void baz();
       union baz; // another elaborated type specifer; another forward declaration
                  // note: the class has the same name as the function void baz()
       union foo {
           long l;
           union baz* b; // elaborated type specifier refers to the class,
                         // not the function of the same name
       };

