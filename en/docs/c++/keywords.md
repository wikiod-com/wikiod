---
title: "Keywords"
slug: "keywords"
draft: false
images: []
weight: 9915
type: docs
toc: true
---

Keywords have fixed meaning defined by the C++ standard and cannot be used as identifiers. It is illegal to redefine keywords using the preprocessor in any translation unit that includes a standard library header. However, keywords lose their special meaning inside attributes.

## Syntax
- asm (*string-literal*);
- noexcept(*expression*) // meaning 1
- noexcept(*constant-expression*) // meaning 2
- noexcept // meaning 2
- sizeof *unary-expression*
- sizeof(*type-id*)
- sizeof...(*identifier*) // since C++11
- typename *nested-name-specifier* *identifier* // meaning 1
- typename *nested-name-specifier* template(*opt*) *simple-template-id* // meaning 1
- typename *identifier*(*opt*) // meaning 2
- typename... *identifier*(*opt*) // meaning 2; since C++11
- typename *identifier*(*opt*) = *type-id* // meaning 2
- template <*template-parameter-list*> typename ...(*opt*) *identifier*(*opt*) // meaning 3
- template <*template-parameter-list*> typename *identifier*(*opt*) = *id-expression* // meaning 3

The full list of keywords is as follows:
 * [`alignas`](https://www.wikiod.com/docs/c%2b%2b/9249/alignment/17909/controlling-alignment) (since C++11)
 * [`alignof`](https://www.wikiod.com/docs/c%2b%2b/9249/alignment/17475/querying-the-alignment-of-a-type) (since C++11)
 * [`asm`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18214/asm)
 * `auto`: [since C++11](https://www.wikiod.com/docs/c%2b%2b/7863/type-deduction/25567/auto-type-deduction), [before C++11](https://www.wikiod.com/docs/c%2b%2b/9225/storage-class-specifiers/28629/auto)
 * [`bool`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18416/bool)
 * [`break`](https://www.wikiod.com/docs/c%2b%2b/7841/iteration/18476/break)
 * [`case`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18489/case)
 * [`catch`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18492/catch)
 * [`char`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18494/char)
 * [`char16_t`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18501/char16-t) (since C++11)
 * [`char32_t`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18502/char32-t) (since C++11)
 * [`class`](https://www.wikiod.com/docs/c%2b%2b/7838/type-keywords/18504/class)
 * [`const`](https://www.wikiod.com/docs/c%2b%2b/7840/variable-declaration-keywords/18509/const)
 * [`constexpr`](https://www.wikiod.com/docs/c%2b%2b/3899/constexpr) (since C++11)
 * [`const_cast`](https://www.wikiod.com/docs/c%2b%2b/3090/explicit-type-conversions/11225/casting-away-constness)
 * [`continue`](https://www.wikiod.com/docs/c%2b%2b/7841/iteration/18512/continue)
 * [`decltype`](https://www.wikiod.com/docs/c%2b%2b/7840/variable-declaration-keywords/18513/decltype) (since C++11)
 * [`default`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18514/default)
 * `delete` [for memory management](https://www.wikiod.com/docs/c%2b%2b/2873/memory-management), [for functions](https://www.wikiod.com/docs/c%2b%2b/206/getting-started-with-c-language/25460/function) (since C++11)
 * [`do`](https://www.wikiod.com/docs/c%2b%2b/7841/iteration/18544/do)
 * [`double`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18640/double)
 * [`dynamic_cast`](https://www.wikiod.com/docs/c%2b%2b/3090/explicit-type-conversions/10518/base-to-derived-conversion)
 * [`else`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18548/else)
 * [`enum`](https://www.wikiod.com/docs/c%2b%2b/7838/type-keywords/18566/enum)
 * [`explicit`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18568/explicit)
 * [`export`](https://www.wikiod.com/docs/c%2b%2b/460/templates)
 * `extern` [as declaration specifier](https://www.wikiod.com/docs/c%2b%2b/9225/storage-class-specifiers), [in linkage specification](https://www.wikiod.com/docs/c%2b%2b/9268/linkage-specifications), [for templates](https://www.wikiod.com/docs/c%2b%2b/460/templates/28734/explicit-instantiation)
 * [`false`](https://www.wikiod.com/docs/c%2b%2b/7836/literal-keywords/18638/false)
 * [`float`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18639/float)
 * [`for`](https://www.wikiod.com/docs/c%2b%2b/7841/iteration/18641/for)
 * [`friend`](https://www.wikiod.com/docs/c%2b%2b/508/classes-structures/7872/friendship)
 * [`goto`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18643/goto)
 * [`if`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18547/if)
 * `inline` [for functions](https://www.wikiod.com/docs/c%2b%2b/7150/inline-functions), [for namespaces](https://www.wikiod.com/docs/c%2b%2b/495/namespaces/4556/inline-namespace) (since C++11), [for variables](https://www.wikiod.com/docs/c%2b%2b/9265/inline-variables) (since C++17)  
 * [`int`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/17264/int)
 * [`long`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18645/long)
 * [`mutable`](https://www.wikiod.com/docs/c%2b%2b/9225/storage-class-specifiers/18647/mutable)
 * [`namespace`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18653/namespace)
 * [`new`](https://www.wikiod.com/docs/c%2b%2b/2873/memory-management)
 * [`noexcept`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18664/noexcept) (since C++11)
 * [`nullptr`](https://www.wikiod.com/docs/c%2b%2b/7836/literal-keywords/18669/nullptr) (since C++11)
 * [`operator`](https://www.wikiod.com/docs/c%2b%2b/562/operator-overloading)
 * [`private`](https://www.wikiod.com/docs/c%2b%2b/508/classes-structures/1668/access-specifiers)
 * [`protected`](https://www.wikiod.com/docs/c%2b%2b/508/classes-structures/1668/access-specifiers)
 * [`public`](https://www.wikiod.com/docs/c%2b%2b/508/classes-structures/1668/access-specifiers)    
 * [`register`](https://www.wikiod.com/docs/c%2b%2b/9225/storage-class-specifiers/18681/register    )
 * [`reinterpret_cast`](https://www.wikiod.com/docs/c%2b%2b/3090/explicit-type-conversions)
 * [`return`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18683/return)
 * [`short`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/18646/short)
 * [`signed`](https://www.wikiod.com/docs/c%2b%2b/7840/variable-declaration-keywords/18685/signed)
 * [`sizeof`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18687/sizeof)
 * [`static`](https://www.wikiod.com/docs/c%2b%2b/9225/storage-class-specifiers/18689/static)
 * [`static_assert`](https://www.wikiod.com/docs/c%2b%2b/3822/static-assert) (since C++11)
 * [`static_cast`](https://www.wikiod.com/docs/c%2b%2b/3090/explicit-type-conversions)
 * [`struct`](https://www.wikiod.com/docs/c%2b%2b/7838/type-keywords/18505/struct)
 * [`switch`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18490/switch)
 * [`template`](https://www.wikiod.com/docs/c%2b%2b/460/templates)
 * [`this`](https://www.wikiod.com/docs/c%2b%2b/7836/literal-keywords/18758/this)
 * [`thread_local`](https://www.wikiod.com/docs/c%2b%2b/699/threading/18759/thread-local-storage) (since C++11)
 * [`throw`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18765/throw)
 * [`true`](https://www.wikiod.com/docs/c%2b%2b/7836/literal-keywords/18637/true)
 * [`try`](https://www.wikiod.com/docs/c%2b%2b/7837/flow-control/18781/try)
 * [`typedef`](https://www.wikiod.com/docs/c%2b%2b/9328/typedef-and-type-aliases)
 * [`typeid`](https://www.wikiod.com/docs/c%2b%2b/3129/rtti-run-time-type-information)
 * [`typename`](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/19075/typename)
 * [`union`](https://www.wikiod.com/docs/c%2b%2b/7838/type-keywords/19092/union)
 * [`unsigned`](https://www.wikiod.com/docs/c%2b%2b/7840/variable-declaration-keywords/18686/unsigned)
 * `using` [to redeclare a name](https://www.wikiod.com/docs/c%2b%2b/9301/using-declaration), [to alias a namespace](https://www.wikiod.com/docs/c%2b%2b/495/namespaces/28691/namespace-alias), [to alias a type](https://www.wikiod.com/docs/c%2b%2b/9328/typedef-and-type-aliases)
 * `virtual` [for functions](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/19097/virtual), [for base classes](https://www.wikiod.com/docs/c%2b%2b/508/classes-structures/1670/virtual-inheritance)
 * [`void`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/19098/void)
 * [`volatile`](https://www.wikiod.com/docs/c%2b%2b/7840/variable-declaration-keywords/19102/volatile)
 * [`wchar_t`](https://www.wikiod.com/docs/c%2b%2b/7839/basic-type-keywords/19113/wchar-t)
 * [`while`](https://www.wikiod.com/docs/c%2b%2b/7841/iteration/25514/while)

The tokens `final` and `override` are not keywords. They may be used as identifiers and have special meaning only in certain contexts.

The tokens `and`, `and_eq`, `bitand`, `bitor`, `compl`, `not`, `not_eq`, `or`, `or_eq`, `xor`, and `xor_eq` are alternative spellings of `&&`, `&=`, `&`, `|`, `~`, `!`, `!=`, `||`, `|=`, `^`, and `^=`, respectively. The standard does not treat them as keywords, but they are keywords for all intents and purposes, since it is impossible to redefine them or use them to mean anything other than the operators they represent.

The following topics contain detailed explanations of many of the keywords in C++, which serve fundamental purposes such as naming basic types or controlling the flow of execution.
* [Basic Type Keywords][1]
* [Flow Control][2]
* [Iteration][3]
* [Literal Keywords][4]
* [Type Keywords][5]
* [Variable Declaration Keywords][6]
* [Classes/Structures][7]
* [Storage class specifiers][8]

  [1]: https://www.wikiod.com/docs/c%2B%2B/7839/basic-type-keywords
  [2]: https://www.wikiod.com/docs/c%2B%2B/7837/flow-control
  [3]: https://www.wikiod.com/docs/c%2B%2B/7841/iteration
  [4]: https://www.wikiod.com/docs/c%2B%2B/7836/literal-keywords
  [5]: https://www.wikiod.com/docs/c%2B%2B/7838/type-keywords
  [6]: https://www.wikiod.com/docs/c%2B%2B/7840/variable-declaration-keywords
  [7]: https://www.wikiod.com/docs/c%2b%2b/508/classes-structures    
  [8]: https://www.wikiod.com/docs/c%2b%2b/9225/storage-class-specifiers    

## asm
The `asm` keyword takes a single operand, which must be a string literal. It has an implementation-defined meaning, but is typically passed to the implementation's assembler, with the assembler's output being incorporated into the translation unit.

The `asm` statement is a *definition*, not an *expression*, so it may appear either at block scope or namespace scope (including global scope). However, since inline assembly cannot be constrained by the rules of the C++ language, `asm` may not appear inside a `constexpr` function.

Example:

    [[noreturn]] void halt_system() {
        asm("hlt");
    }

## explicit
1. When applied to a single-argument constructor, prevents that constructor from being used to perform implicit conversions.

       class MyVector {
         public:
           explicit MyVector(uint64_t size);
       };
       MyVector v1(100);  // ok
       uint64_t len1 = 100;
       MyVector v2{len1}; // ok, len1 is uint64_t
       int len2 = 100;
       MyVector v3{len2}; // ill-formed, implicit conversion from int to uint64_t

   Since C++11 introduced initializer lists, in C++11 and later, `explicit` can be applied to a constructor with any number of arguments, with the same meaning as in the single-argument case.

       struct S {
           explicit S(int x, int y);
       };
       S f() {
           return {12, 34};  // ill-formed
           return S{12, 34}; // ok
       }

<!-- if version [gte C++11] -->
2. When applied to a conversion function, prevents that conversion function from being used to perform implicit conversions.

       class C {
           const int x;
         public:
           C(int x) : x(x) {}
           explicit operator int() { return x; }
       };
       C c(42);
       int x = c;                   // ill-formed
       int y = static_cast<int>(c); // ok; explicit conversion
<!-- end version if -->

## noexcept
<!-- if version [gte C++11] -->
1. A unary operator that determines whether the evaluation of its operand can propagate an exception. Note that the bodies of called functions are not examined, so `noexcept` can yield false negatives. The operand is not evaluated.

       #include <iostream>
       #include <stdexcept>
       void foo() { throw std::runtime_error("oops"); }
       void bar() {}
       struct S {};
       int main() {
           std::cout << noexcept(foo()) << '\n'; // prints 0
           std::cout << noexcept(bar()) << '\n'; // prints 0
           std::cout << noexcept(1 + 1) << '\n'; // prints 1
           std::cout << noexcept(S()) << '\n';   // prints 1
       }

   In this example, even though `bar()` can never throw an exception, `noexcept(bar())` is still false because the fact that `bar()` cannot propagate an exception has not been explicitly specified.

2. When declaring a function, specifies whether or not the function can propagate an exception. Alone, it declares that the function cannot propagate an exception. With a parenthesized argument, it declares that the function can or cannot propagate an exception depending on the truth value of the argument.

       void f1() { throw std::runtime_error("oops"); }
       void f2() noexcept(false) { throw std::runtime_error("oops"); }
       void f3() {}
       void f4() noexcept {}
       void f5() noexcept(true) {}
       void f6() noexcept {
           try {
               f1();
           } catch (const std::runtime_error&) {}
       }

   In this example, we have declared that `f4`, `f5`, and `f6` cannot propagate exceptions. (Although an exception can be thrown during execution of `f6`, it is caught and not allowed to propagate out of the function.) We have declared that `f2` may propagate an exception. When the `noexcept` specifier is omitted, it is equivalent to `noexcept(false)`, so we have implicitly declared that `f1` and `f3` may propagate exceptions, even though exceptions cannot actually be thrown during the execution of `f3`.

<!-- if version [gte C++17] -->
Whether or not a function is `noexcept` is part of the function's type: that is, in the example above, `f1`, `f2`, and `f3` have different types from `f4`, `f5`, and `f6`. Therefore, `noexcept` is also significant in function pointers, template arguments, and so on.

    void g1() {}
    void g2() noexcept {}
    void (*p1)() noexcept = &g1; // ill-formed, since g1 is not noexcept
    void (*p2)() noexcept = &g2; // ok; types match
    void (*p3)() = &g1;          // ok; types match
    void (*p4)() = &g2;          // ok; implicit conversion
<!-- end version if -->
<!-- end version if -->

## typename
1. When followed by a qualified name, `typename` specifies that it is the name of a type. This is often required in templates, in particular, when the nested name specifier is a dependent type other than the current instantiation. In this example, `std::decay<T>` depends on the template parameter `T`, so in order to name the nested type `type`, we need to prefix the entire qualified name with `typename`. For more deatils, see [Where and why do I have to put the "template" and "typename" keywords?](http://stackoverflow.com/questions/610245/where-and-why-do-i-have-to-put-the-template-and-typename-keywords)

       template <class T>
       auto decay_copy(T&& r) -> typename std::decay<T>::type;

2. Introduces a type parameter in the declaration of a [template](https://www.wikiod.com/docs/c%2b%2b/460/templates). In this context, it is interchangeable with `class`.

       template <typename T>
       const T& min(const T& x, const T& y) {
           return b < a ? b : a;
       } 

<!-- if version [gte C++17] -->
3. `typename` can also be used when declaring a [template template parameter](https://www.wikiod.com/docs/c%2b%2b/460/templates/10838/template-template-parameters), preceding the name of the parameter, just like `class`.

       template <template <class T> typename U>
       void f() {
           U<int>::do_it();
           U<double>::do_it();
       }
<!-- end version if -->

## sizeof
A unary operator that yields the size in bytes of its operand, which may be either an expression or a type. If the operand is an expression, it is not evaluated. The size is a constant expression of type `std::size_t`.

If the operand is a type, it must be parenthesized.

* It is illegal to apply `sizeof` to a function type.
* It is illegal to apply `sizeof` to an incomplete type, including `void`.
* If sizeof is applied to a reference type `T&` or `T&&`, it is equivalent to `sizeof(T)`.
* When `sizeof` is applied to a class type, it yields the number of bytes in a complete object of that type, including any padding bytes in the middle or at the end. Therefore, a `sizeof` expression can never have a value of 0. See [layout of object types](https://www.wikiod.com/docs/c%2b%2b/9329/layout-of-object-types) for more details.
* The `char`, `signed char`, and `unsigned char` types have a size of 1. Conversely, a byte is defined to be the amount of memory required to store a `char` object. It does not necessarily mean 8 bits, as some systems have `char` objects longer than 8 bits.

If *expr* is an expression, `sizeof(`*expr*`)` is equivalent to `sizeof(T)` where `T` is the type of *expr.*

    int a[100];
    std::cout << "The number of bytes in `a` is: " << sizeof a;
    memset(a, 0, sizeof a); // zeroes out the array

<!-- if version [gte C++11] -->
The `sizeof...` operator yields the number of elements in a parameter pack.

    template <class... T>
    void f(T&&...) {
        std::cout << "f was called with " << sizeof...(T) << " arguments\n";
    }
<!-- end version if -->


## Different keywords
>**void C++**

 1. When used as a function return type, the void keyword specifies that the function does not return a value. When used for a function's parameter list, void specifies that the function takes no parameters. When used in the declaration of a pointer, void specifies that the pointer is "universal."
 2. If a pointer's type is void *, the pointer can point to any variable that is not declared with the const or volatile keyword. A void pointer cannot be dereferenced unless it is cast to another type. A void pointer can be converted into any other type of data pointer.
 3. A void pointer can point to a function, but not to a class member in C++.

        void vobject;   // C2182  
        void *pv;   // okay  
        int *pint; int i;  
        int main() {  
        pv = &i;  
           // Cast optional in C required in C++  
        pint = (int *)pv;  
        

   

>**Volatile C++**

 1. A type qualifier that you can use to declare that an object can be modified in the program by the hardware.

        volatile declarator ;

>**virtual C++**

 1. The virtual keyword declares a virtual function or a virtual base class.

        virtual [type-specifiers] member-function-declarator  
        virtual [access-specifier] base-class-name 

**Parameters**

 1. **type-specifiers**   Specifies the return type of the virtual member function.

 2. **member-function-declarator**   Declares a member function.

 3. **access-specifier**   Defines the level of access to the base class, public, protected or private. Can appear before or after the virtual keyword.

 4. **base-class-name**  Identifies a previously declared class type

>**this pointer**

 1. The this pointer is a pointer accessible only within the nonstatic member functions of a class, struct, or union type. It points to the object for which the member function is called. Static member functions do not have a this pointer.

        this->member-identifier  
   

An object's this pointer is not part of the object itself; it is not reflected in the result of a sizeof statement on the object. Instead, when a nonstatic member function is called for an object, the address of the object is passed by the compiler as a hidden argument to the function. For example, the following function call:

    
    
    myDate.setMonth( 3 );  
    
    can be interpreted this way:
    
    
    setMonth( &myDate, 3 );  
    
    The object's address is available from within the member function as the this pointer. Most uses of this are implicit. It is legal, though unnecessary, to explicitly use this when referring to members of the class. For example:
    
    
    void Date::setMonth( int mn )  
    {  
       month = mn;            // These three statements  
       this->month = mn;      // are equivalent  
       (*this).month = mn;  
    }  
    
    The expression *this is commonly used to return the current object from a member function:
    
    
    return *this;  
    
    The this pointer is also used to guard against self-reference:
    
    
    if (&Object != this) {  
    // do not execute in cases of self-reference 


>**try, throw, and catch Statements (C++)**

 1. To implement exception handling in C++, you use try, throw, and catch expressions.
 2. First, use a try block to enclose one or more statements that might throw an exception.
 3. A throw expression signals that an exceptional condition—often, an error—has occurred in a try block. You can use an object of any type as the operand of a throw expression. Typically, this object is used to communicate information about the error. In most cases, we recommend that you use the std::exception class or one of the derived classes that are defined in the standard library. If one of those is not appropriate, we recommend that you derive your own exception class from std::exception.
 4. To handle exceptions that may be thrown, implement one or more catch blocks immediately following a try block. Each catch block specifies the type of exception it can handle.
 

        MyData md;  
    try {  
       // Code that could throw an exception  
       md = GetNetworkResource();  
    }  
    catch (const networkIOException& e) {  
       // Code that executes when an exception of type  
       // networkIOException is thrown in the try block  
       // ...  
       // Log error message in the exception object  
       cerr << e.what();  
    }  
    catch (const myDataFormatException& e) {  
       // Code that handles another exception type  
       // ...  
       cerr << e.what();  
    }  
      
    // The following syntax shows a throw expression  
    MyData GetNetworkResource()  
    {  
       // ...  
       if (IOSuccess == false)  
          throw networkIOException("Unable to connect");  
       // ...  
       if (readError)  
          throw myDataFormatException("Format error");   
       // ...  
    }

>The code after the try clause is the guarded section of code. The throw expression throws—that is, raises—an exception. The code block after the catch clause is the exception handler. This is the handler that catches the exception that's thrown if the types in the throw and catch expressions are compatible.

        try {  
       throw CSomeOtherException();  
    }  
    catch(...) {  
       // Catch all exceptions – dangerous!!!  
       // Respond (perhaps only partially) to the exception, then  
       // re-throw to pass the exception to some other handler  
       // ...  
       throw;  
    }

>**friend (C++)**

 1. In some circumstances, it is more convenient to grant member-level access to functions that are not members of a class or to all members in a separate class. Only the class implementer can declare who its friends are. A function or class cannot declare itself as a friend of any class. In a class definition, use the friend keyword and the name of a non-member function or other class to grant it access to the private and protected members of your class. In a template definition, a type parameter can be declared as a friend.
 
 2. If you declare a friend function that was not previously declared, that function is exported to the enclosing nonclass scope.

        class friend F  
        friend F;
        class ForwardDeclared;// Class name is known.  
        class HasFriends  
        {  
           friend int ForwardDeclared::IsAFriend();// C2039 error expected  
        };  

>**friend functions**

 1. A friend function is a function that is not a member of a class but has access to the class's private and protected members.Friend functions are not considered class members; they are normal external functions that are given special access privileges.
 2.  Friends are not in the class's scope, and they are not called using the member-selection operators (. and –>) unless they are members of another class.
 3. A friend function is declared by the class that is granting access. The friend declaration can be placed anywhere in the class declaration. It is not affected by the access control keywords.

        #include <iostream>  
      
        using namespace std;  
        class Point  
        {  
            friend void ChangePrivate( Point & );  
        public:  
            Point( void ) : m_i(0) {}  
            void PrintPrivate( void ){cout << m_i << endl; }  
      
        private:  
        int m_i;  
        };  
      
        void ChangePrivate ( Point &i ) { i.m_i++; }  
      
        int main()  
        {  
           Point sPoint;  
           sPoint.PrintPrivate();  
           ChangePrivate(sPoint);  
           sPoint.PrintPrivate();  
            // Output: 0  
                   1  
        }  

**Class members as friends**

    class B;  
  
    class A {  
    public:  
       int Func1( B& b );  
  
    private:  
       int Func2( B& b );  
    };  
  
    class B {  
    private:  
    int _b;  
  
       // A::Func1 is a friend function to class B  
       // so A::Func1 has access to all members of B  
       friend int A::Func1( B& );  
    };  
  
    int A::Func1( B& b ) { return b._b; }   // OK  
    int A::Func2( B& b ) { return b._b; }   // C2248  




