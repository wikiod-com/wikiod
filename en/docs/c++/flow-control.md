---
title: "Flow Control"
slug: "flow-control"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

Check out the [loops topic][1] for the different kind of loops.


  [1]: https://www.wikiod.com/docs/c%2B%2B/589/loops

## case
Introduces a case label of a switch statement. The operand must be a constant expression and match the switch condition in type. When the switch statement is executed, it will jump to the case label with operand equal to the condition, if any.

    char c = getchar();
    bool confirmed;
    switch (c) {
      case 'y':
        confirmed = true;
        break;
      case 'n':
        confirmed = false;
        break;
      default:
        std::cout << "invalid response!\n";
        abort();
    }

## switch
According to the C++ standard,
> The `switch` statement causes control to be transferred to one of several statements depending on the value
of a condition.

The keyword `switch` is followed by a parenthesized condition and a block, which may contain `case` labels and an optional `default` label. When the switch statement is executed, control will be transferred either to a `case` label with a value matching that of the condition, if any, or to the `default` label, if any.

The condition must be an expression or a declaration, which has either integer or enumeration type, or a class type with a conversion function to integer or enumeration type. 

    char c = getchar();
    bool confirmed;
    switch (c) {
      case 'y':
        confirmed = true;
        break;
      case 'n':
        confirmed = false;
        break;
      default:
        std::cout << "invalid response!\n";
        abort();
    }

## catch
The `catch` keyword introduces an exception handler, that is, a block into which control will be transferred when an exception of compatible type is thrown. The `catch` keyword is followed by a parenthesized *exception declaration*, which is similar in form to a function parameter declaration: the parameter name may be omitted, and the ellipsis `...` is allowed, which matches any type. The exception handler will only handle the exception if its declaration is compatible with the type of the exception. For more details, see [catching exceptions](https://www.wikiod.com/docs/c%2b%2b/1354/exceptions/4414/catching-exceptions#t=201608050644277417742).

    try {
        std::vector<int> v(N);
        // do something
    } catch (const std::bad_alloc&) {
        std::cout << "failed to allocate memory for vector!" << std::endl;
    } catch (const std::runtime_error& e) {
        std::cout << "runtime error: " << e.what() << std::endl;
    } catch (...) {
        std::cout << "unexpected exception!" << std::endl;
        throw;
    }

## default
In a switch statement, introduces a label that will be jumped to if the condition's value is not equal to any of the case labels' values.

    char c = getchar();
    bool confirmed;
    switch (c) {
      case 'y':
        confirmed = true;
        break;
      case 'n':
        confirmed = false;
        break;
      default:
        std::cout << "invalid response!\n";
        abort();
    }

<!-- if version [gte C++11] -->
Defines a default constructor, copy constructor, move constructor, destructor, copy assignment operator, or move assignment operator to have its default behaviour.

    class Base {
        // ...
        // we want to be able to delete derived classes through Base*,
        // but have the usual behaviour for Base's destructor.
        virtual ~Base() = default;
    };

<!-- end version if -->

## if
Introduces an if statement. The keyword `if` must be followed by a parenthesized condition, which can be either an expression or a declaration. If the condition is truthy, the substatement after the condition will be executed.

    int x;
    std::cout << "Please enter a positive number." << std::endl;
    std::cin >> x;
    if (x <= 0) {
        std::cout << "You didn't enter a positive number!" << std::endl;
        abort();
    }

## else
The first substatement of an if statement may be followed by the keyword `else`. The substatement after the `else` keyword will be executed when the condition is falsey (that is, when the first substatement is not executed).

    int x;
    std::cin >> x;
    if (x%2 == 0) {
        std::cout << "The number is even\n";
    } else {
        std::cout << "The number is odd\n";
    }

## goto
Jumps to a labelled statement, which must be located in the current function.

    bool f(int arg) {
        bool result = false;
        hWidget widget = get_widget(arg);
        if (!g()) {
            // we can't continue, but must do cleanup still
            goto end;
        }
        // ...
        result = true;
      end:
        release_widget(widget);
        return result;
    }

## return
Returns control from a function to its caller.

If `return` has an operand, the operand is converted to the function's return type, and the converted value is returned to the caller.

    int f() {
        return 42;
    }
    int x = f(); // x is 42
    int g() {
        return 3.14;
    }
    int y = g(); // y is 3

If `return` does not have an operand, the function must have `void` return type. As a special case, a `void`-returning function can also return an expression if the expression has type `void`.

    void f(int x) {
        if (x < 0) return;
        std::cout << sqrt(x);
    }
    int g() { return 42; }
    void h() {
        return f(); // calls f, then returns
        return g(); // ill-formed
    }

When `main` returns, `std::exit` is implicitly called with the return value, and the value is thus returned to the execution environment. (However, returning from `main`  destroys automatic local variables, while calling `std::exit` directly does not.)

    int main(int argc, char** argv) {
        if (argc < 2) {
            std::cout << "Missing argument\n";
            return EXIT_FAILURE; // equivalent to: exit(EXIT_FAILURE);
        }
    }

## throw
1. When `throw` occurs in an expression with an operand, its effect is to throw an [exception](https://www.wikiod.com/docs/c%2b%2b/1354/exceptions), which is a copy of the operand.

       void print_asterisks(int count) {
           if (count < 0) {
               throw std::invalid_argument("count cannot be negative!");
           }
           while (count--) { putchar('*'); }
       }

2. When `throw` occurs in an expression without an operand, its effect is to [rethrow the current exception](https://www.wikiod.com/docs/c%2b%2b/1354/exceptions/5574/rethrow-propagate-exception). If there is no current exception, `std::terminate` is called.

       try {
           // something risky
       } catch (const std::bad_alloc&) {
           std::cerr << "out of memory" << std::endl;
       } catch (...) {
           std::cerr << "unexpected exception" << std::endl;
           // hope the caller knows how to handle this exception
           throw;
       }

3. When `throw` occurs in a function declarator, it introduces a dynamic exception specification, which lists the types of exceptions that the function is allowed to propagate.

       // this function might propagate a std::runtime_error,
       // but not, say, a std::logic_error
       void risky() throw(std::runtime_error);
       // this function can't propagate any exceptions
       void safe() throw();

   Dynamic exception specifications are deprecated as of C++11.

Note that the first two uses of `throw` listed above constitute expressions rather than statements. (The type of a throw expression is `void`.) This makes it possible to nest them within expressions, like so:

    unsigned int predecessor(unsigned int x) {
        return (x > 0) ? (x - 1) : (throw std::invalid_argument("0 has no predecessor"));
    }

## try
The keyword `try` is followed by a block, or by a constructor initializer list and then a block (see [here](https://www.wikiod.com/docs/c%2b%2b/1354/exceptions/6692/function-try-blocks-in-constructor)). The try block is followed by one or more [catch blocks](https://www.wikiod.com/docs/c%2b%2b/4891/keywords/18492/catch). If an [exception](https://www.wikiod.com/docs/c%2b%2b/1354/exceptions) propagates out of the try block, each of the corresponding catch blocks after the try block has the opportunity to handle the exception, if the types match.

    std::vector<int> v(N);     // if an exception is thrown here,
                               // it will not be caught by the following catch block
    try {
        std::vector<int> v(N); // if an exception is thrown here,
                               // it will be caught by the following catch block
        // do something with v
    } catch (const std::bad_alloc&) {
        // handle bad_alloc exceptions from the try block
    }    

## Conditional Structures: if, if..else


## Jump statements : break, continue, goto, exit.


