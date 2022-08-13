---
title: "Virtual Member Functions"
slug: "virtual-member-functions"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Syntax
- virtual void f();
- virtual void g() = 0;

- // C++11 or later:
  - virtual void h() override;
  - void i() override;
  - virtual void j() final;
  - void k() final;

- Only non-static, non-template member functions can be `virtual`.
- If you are using C++11 or later, it is recommended to use `override` when overriding a virtual member function from a base class.
- [Polymorphic base classes often have virtual destructors to allow a derived object to be deleted through a pointer to the base class][1]. If the destructor were not virtual, such an operation leads to [undefined behavior](https://www.wikiod.com/docs/c%2b%2b/1812/undefined-behavior)<sup>[expr.delete] §5.3.5/3 </sup>.


  [1]: https://www.wikiod.com/docs/c%2B%2B/1717/polymorphism/20178/polymorphism-destructors

## Using override with virtual in C++11 and later
The specifier `override` has a special meaning in C++11 onwards, if appended at the end of function signature. This signifies that a function is 

- Overriding the function present in base class &
- The Base class function is `virtual` 

There is no `run time` significance of this specifier as is mainly meant as an indication for compilers

The example below will demonstrate the change in behaviour with our without using override.

Without `override`:
```
#include <iostream>

struct X {
    virtual void f() { std::cout << "X::f()\n"; }
};

struct Y : X {
    // Y::f() will not override X::f() because it has a different signature,
    // but the compiler will accept the code (and silently ignore Y::f()).
    virtual void f(int a) { std::cout << a << "\n"; }
};
```
With `override`:
```
#include <iostream>

struct X {
    virtual void f() { std::cout << "X::f()\n"; }
};

struct Y : X {
    // The compiler will alert you to the fact that Y::f() does not
    // actually override anything.
    virtual void f(int a) override { std::cout << a << "\n"; }
};
```

Note that `override` is not a keyword, but a special identifier which only may appear in function signatures. In all other contexts `override`  still may be used as an identifier:

```
void foo() {
    int override = 1; // OK.
    int virtual = 2;  // Compilation error: keywords can't be used as identifiers.
}
```

## Virtual vs non-virtual member functions
With virtual member functions:
```
#include <iostream>

struct X {
    virtual void f() { std::cout << "X::f()\n"; }
};

struct Y : X {
    // Specifying virtual again here is optional
    // because it can be inferred from X::f().
    virtual void f() { std::cout << "Y::f()\n"; } 
};

void call(X& a) {
    a.f();
}

int main() {
    X x;
    Y y;
    call(x); // outputs "X::f()"
    call(y); // outputs "Y::f()"
}

```

Without virtual member functions:
```
#include <iostream>

struct X {
   void f() { std::cout << "X::f()\n"; }
};

struct Y : X {
   void f() { std::cout << "Y::f()\n"; }
};

void call(X& a) {
    a.f();
}

int main() {
    X x;
    Y y;
    call(x); // outputs "X::f()"
    call(y); // outputs "X::f()"
}

```

## Final virtual functions
C++11 introduced `final` specifier which forbids method overriding if appeared in method signature:

    class Base {
    public:
        virtual void foo() {
            std::cout << "Base::Foo\n";
        }
    };
    
    class Derived1 : public Base {
    public:
        // Overriding Base::foo
        void foo() final {
            std::cout << "Derived1::Foo\n";
        }
    };
    
    class Derived2 : public Derived1 {
    public:
        // Compilation error: cannot override final method
        virtual void foo() {
            std::cout << "Derived2::Foo\n";
        }
    };



The specifier `final` can only be used with `virtual' member function and can't be applied to non-virtual member functions

Like `final`, there is also an specifier caller 'override' which prevent overriding of `virtual` functions in the derived class.

The specifiers `override` and `final` may be combined together to have desired effect:

    class Derived1 : public Base {
    public:
        void foo() final override {
            std::cout << "Derived1::Foo\n";
        }
    };

## Behaviour of virtual functions in constructors and destructors
The behaviour of virtual functions in constructors and destructors is often confusing when first encountered.

    #include <iostream>
    using namespace std;

    class base { 
    public:
        base() { f("base constructor"); }
        ~base() { f("base destructor"); }

        virtual const char* v() { return "base::v()"; }

        void f(const char* caller) { 
            cout << "When called from " << caller << ", "  << v() << " gets called.\n"; 
        }        
    };

    class derived : public base {
    public:
        derived() { f("derived constructor"); }
        ~derived() { f("derived destructor"); }

        const char* v() override { return "derived::v()"; }

    };

    int main() {
         derived d;
    }

**Output:**
> When called from base constructor, base::v() gets called.  
When called from derived constructor, derived::v() gets called.  
When called from derived destructor, derived::v() gets called.  
When called from base destructor, base::v() gets called.  

The reasoning behind this is that the derived class may define additional members which are not yet initialized (in the constructor case) or already destroyed (in the destructor case), and calling its member functions would be unsafe. Therefore during construction and destruction of C++ objects, the _dynamic_ type of `*this` is considered to be the constructor's or destructor's class and not a more-derived class.

**Example:**

    #include <iostream>
    #include <memory>
    
    using namespace std;
    class base {
    public:
        base()
        {
            std::cout << "foo is " << foo() << std::endl;
        }
        virtual int foo() { return 42; }
    };
    
    class derived : public base {
        unique_ptr<int> ptr_;
    public:
        derived(int i) : ptr_(new int(i*i)) { }
        // The following cannot be called before derived::derived due to how C++ behaves, 
        // if it was possible... Kaboom!
        int foo() override   { return *ptr_; } 
    };
    
    int main() {
        derived d(4);
    }



## Pure virtual functions
We can also specify that a `virtual` function is _pure virtual_ (abstract), by appending `= 0` to the declaration.  Classes with one or more pure virtual functions are considered to be abstract, and cannot be instantiated; only derived classes which define, or inherit definitions for, all pure virtual functions can be instantiated.

    struct Abstract {
        virtual void f() = 0;
    };

    struct Concrete {
        void f() override {}
    };

    Abstract a; // Error.
    Concrete c; // Good.

Even if a function is specified as pure virtual, it can be given a default implementation.  Despite this, the function will still be considered abstract, and derived classes will have to define it before they can be instantiated.  In this case, the derived class' version of the function is even allowed to call the base class' version.

    struct DefaultAbstract {
        virtual void f() = 0;
    };
    void DefaultAbstract::f() {}

    struct WhyWouldWeDoThis : DefaultAbstract {
        void f() override { DefaultAbstract::f(); }
    };

There are a couple of reasons why we might want to do this:
* If we want to create a class that can't itself be instantiated, but doesn't prevent its derived classes from being instantiated, we can declare the destructor as pure virtual.  Being the destructor, it must be defined anyways, if we want to be able to deallocate the instance.  And [as the destructor is most likely already virtual to prevent memory leaks during polymorphic use][1], we won't incur an unnecessary performance hit from declaring another function `virtual`.  This can be useful when making interfaces.

        struct Interface {
            virtual ~Interface() = 0;
        };
        Interface::~Interface() = default;

        struct Implementation : Interface {};
        // ~Implementation() is automatically defined by the compiler if not explicitly
        //  specified, meeting the "must be defined before instantiation" requirement.

* If most or all implementations of the pure virtual function will contain duplicate code, that code can instead be moved to the base class version, making the code easier to maintain.

        class SharedBase {
            State my_state;
            std::unique_ptr<Helper> my_helper;
            // ...

          public:
            virtual void config(const Context& cont) = 0;
            // ...
        };
        /* virtual */ void SharedBase::config(const Context& cont) {
            my_helper = new Helper(my_state, cont.relevant_field);
            do_this();
            and_that();
        }

        class OneImplementation : public SharedBase {
            int i;
            // ...

          public:
            void config(const Context& cont) override;
            // ...
        };
        void OneImplementation::config(const Context& cont) /* override */ {
            my_state = { cont.some_field, cont.another_field, i };
            SharedBase::config(cont);
            my_unique_setup();
        };

        // And so on, for other classes derived from SharedBase.


  [1]: https://www.wikiod.com/docs/c%2B%2B/1717/polymorphism/20178/polymorphism-destructors

