---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9669
type: docs
toc: true
---

A loop statement executes a group of statements repeatedly until a condition is met.
There are 3 types of primitive loops in C++: for, while, and do...while.

## Syntax
- while (*condition*) *statement* ;
- do *statement* while (*expression*) ;
- for (*for-init-statement* ; *condition* ; *expression*) *statement* ;
- for (*for-range-declaration* : *for-range-initializer*) *statement* ;
- break ;
- continue ;

[`algorithm`][1] calls are generally preferable to hand-written loops.

If you want something an algorithm already does (or something very similar), the algorithm call is clearer, often more efficient and less error prone.

If you need a loop that does something fairly simple (but would require a confusing tangle of binders and adapters if you were using an algorithm), then just write the loop.


  [1]: http://en.cppreference.com/w/cpp/algorithm

## Range-Based For
<!-- if version [gte C++11] -->
`for` loops can be used to iterate over the elements of a iterator-based range, without using a numeric index or directly accessing the iterators:

    vector<float> v = {0.4f, 12.5f, 16.234f};
    
    for(auto val: v)
    {
        std::cout << val << " ";
    }
    
    std::cout << std::endl;

This will iterate over every element in `v`, with `val` getting the value of the current element. The following statement:

    for (for-range-declaration : for-range-initializer ) statement

is equivalent to:

    {
        auto&& __range = for-range-initializer;
        auto __begin = begin-expr, __end = end-expr;
        for (; __begin != __end; ++__begin) {
            for-range-declaration = *__begin;
            statement
        }
    }

<!-- if version [gte C++17] -->

    {
        auto&& __range = for-range-initializer;
        auto __begin = begin-expr;
        auto __end = end-expr; // end is allowed to be a different type than begin in C++17
        for (; __begin != __end; ++__begin) {
            for-range-declaration = *__begin;
            statement
        }
    }

This change was introduced for the planned support of Ranges TS in C++20.

<!-- end version if -->

In this case, our loop is equivalent to:

    {
        auto&& __range = v;
        auto __begin = v.begin(), __end = v.end();
        for (; __begin != __end; ++__begin) {
            auto val = *__begin;
            std::cout << val << " ";
        }
    }

Note that `auto val` declares a value type, which will be a copy of a value stored in the range (we are copy-initializing it from the iterator as we go). If the values stored in the range are expensive to copy, you may want to use `const auto &val`. You are also not required to use `auto`; you can use an appropriate typename, so long as it is implicitly convertible from the range's value type.

If you need access to the iterator, range-based for cannot help you (not without some effort, at least).

If you wish to reference it, you may do so:

    vector<float> v = {0.4f, 12.5f, 16.234f};
    
    for(float &val: v)
    {
        std::cout << val << " ";
    }

You could iterate on `const` reference if you have `const` container:

    const vector<float> v = {0.4f, 12.5f, 16.234f};
    
    for(const float &val: v)
    {
        std::cout << val << " ";
    }

One would use forwarding references when the sequence iterator returns a proxy object and you need to operate on that object in a non-`const` way. Note: it will most likely confuse readers of your code.

    vector<bool> v(10);
    
    for(auto&& val: v)
    {
        val = true;
    }

The "range" type provided to range-based `for` can be one of the following:

* Language arrays:

      float arr[] = {0.4f, 12.5f, 16.234f};

      for(auto val: arr)
      {
          std::cout << val << " ";
      }

    Note that allocating a dynamic array does not count:

      float *arr = new float[3]{0.4f, 12.5f, 16.234f};

      for(auto val: arr) //Compile error.
      {
          std::cout << val << " ";
      }

* Any type which has member functions `begin()` and `end()`, which return iterators to the elements of the type. The standard library containers qualify, but user-defined types can be used as well:

      struct Rng
      {
          float arr[3];
    
          // pointers are iterators
          const float* begin() const {return &arr[0];}
          const float* end() const   {return &arr[3];}
          float* begin() {return &arr[0];}
          float* end()   {return &arr[3];}
      };

      int main()
      {
          Rng rng = {{0.4f, 12.5f, 16.234f}};
    
          for(auto val: rng)
          {
              std::cout << val << " ";
          }
      }

* Any type which has non-member `begin(type)` and `end(type)` functions which can found via argument dependent lookup, based on `type`. This is useful for creating a range type without having to modify class type itself:

      namespace Mine
      {
          struct Rng {float arr[3];};
    
          // pointers are iterators
          const float* begin(const Rng &rng) {return &rng.arr[0];}
          const float* end(const Rng &rng) {return &rng.arr[3];}
          float* begin(Rng &rng) {return &rng.arr[0];}
          float* end(Rng &rng) {return &rng.arr[3];}
      }
    
      int main()
      {
          Mine::Rng rng = {{0.4f, 12.5f, 16.234f}};
    
          for(auto val: rng)
          {
              std::cout << val << " ";
          }
      }
<!-- end version if -->

## For loop
A `for` loop executes statements in the `loop body`, while the loop `condition` is true. Before the loop `initialization statement` is executed exactly once. After each cycle, the `iteration execution` part is executed. 

A `for` loop is defined as follows:

    for (/*initialization statement*/; /*condition*/; /*iteration execution*/)
    {
        // body of the loop
    }

Explanation of the placeholder statements:

- `initialization statement`: This statement gets executed only once, at the beginning of the `for` loop. You can enter a declaration of multiple variables of one type, such as `int i = 0, a = 2, b = 3`. These variables are only valid in the scope of the loop. Variables defined before the loop with the same name are hidden during execution of the loop.
- `condition`: This statement gets evaluated ahead of each _loop body_ execution, and aborts the loop if it evaluates to `false`.
- `iteration execution`: This statement gets executed after the loop _body_, ahead of the next _condition_ evaluation, unless the `for` loop is aborted in the _body_ (by `break`, `goto`, `return` or an exception being thrown). You can enter multiple statements in the `iteration execution` part, such as `a++, b+=10, c=b+a`.

The rough equivalent of a `for` loop, rewritten as a `while` loop is:

    /*initialization*/
    while (/*condition*/)
    {
        // body of the loop; using 'continue' will skip to increment part below
        /*iteration execution*/
    }

The most common case for using a `for` loop is to execute statements a specific number of times. For example, consider the following:

    for(int i = 0; i < 10; i++) {
        std::cout << i << std::endl;
    }

A valid loop is also:
    
    for(int a = 0, b = 10, c = 20; (a+b+c < 100); c--, b++, a+=c) {
        std::cout << a << " " << b << " " << c << std::endl; 
    }

An example of hiding declared variables before a loop is:

    int i = 99; //i = 99
    for(int i = 0; i < 10; i++) { //we declare a new variable i
        //some operations, the value of i ranges from 0 to 9 during loop execution
    }
    //after the loop is executed, we can access i with value of 99

But if you want to use the already declared variable and not hide it, then omit the declaration part:

    int i = 99; //i = 99
    for(i = 0; i < 10; i++) { //we are using already declared variable i
        //some operations, the value of i ranges from 0 to 9 during loop execution
    }
    //after the loop is executed, we can access i with value of 10

Notes:

- The initialization and increment statements can perform operations unrelated to the condition statement, or nothing at all - if you wish to do so. But for readability reasons, it is best practice to only perform operations directly relevant to the loop.
- A variable declared in the initialization statement is visible only inside the scope of the `for` loop and is released upon termination of the loop.
- Don't forget that the variable which was declared in the `initialization statement` can be modified during the loop, as well as the variable checked in the `condition`.

Example of a loop which counts from 0 to 10:

    for (int counter = 0; counter <= 10; ++counter)
    {
        std::cout << counter << '\n';
    }
    // counter is not accessible here (had value 11 at the end)

Explanation of the code fragments:

- `int counter = 0` initializes the variable `counter` to 0. (This variable can only be used inside of the `for` loop.)
- `counter <= 10` is a Boolean condition that checks whether `counter` is less than or equal to 10. If it is `true`, the loop executes. If it is `false`, the loop ends.
- `++counter` is an increment operation that increments the value of `counter` by 1 ahead of the next condition check.

By leaving all statements empty, you can create an infinite loop:

    // infinite loop
    for (;;)
        std::cout << "Never ending!\n";

The `while` loop equivalent of the above is:

    // infinite loop
    while (true)
        std::cout << "Never ending!\n";

However, an infinite loop can still be left by using the statements `break`, `goto`, or `return` or by throwing an exception.

The next common example of iterating over all elements from an STL collection (e.g., a `vector`) without using the `<algorithm>` header is:

    std::vector<std::string> names = {"Albert Einstein", "Stephen Hawking", "Michael Ellis"};
    for(std::vector<std::string>::iterator it = names.begin(); it != names.end(); ++it) {
        std::cout << *it << std::endl;
    }



## While loop
A `while` loop executes statements repeatedly until the given condition evaluates to `false`. This control statement is used when it is not known, in advance, how many times a block of code is to be executed.

For example, to print all the numbers from 0 up to 9, the following code can be used:

    int i = 0;
    while (i < 10)
    {
        std::cout << i << " ";
        ++i; // Increment counter
    }
    std::cout << std::endl; // End of line; "0 1 2 3 4 5 6 7 8 9" is printed to the console

<!-- if version [gte C++17] -->

Note that since C++17, the first 2 statements can be combined

    while (int i = 0; i < 10)
    //... The rest is the same

<!-- end version if -->

To create an infinite loop, the following construct can be used:

    while (true)
    {
        // Do something forever (however, you can exit the loop by calling 'break'
    }

There is another variant of `while` loops, namely the `do...while` construct. See the [do-while loop example](https://www.wikiod.com/docs/c%2b%2b/589/loops/7821/do-while-loop#t=201607221948036580158) for more information.

## Do-while loop
A _do-while_ loop is very similar to a _while_ loop, except that the condition is checked at the end of each cycle, not at the start. The loop is therefore guaranteed to execute at least once.

The following code will print `0`, as the condition will evaluate to `false` at the end of the first iteration:

    int i =0;
    do
    {
        std::cout << i;
        ++i; // Increment counter
    }
    while (i < 0);
    std::cout << std::endl; // End of line; 0 is printed to the console

Note: Do not forget the semicolon at the end of `while(condition);`, which is needed in the _do-while_ construct.

In contrast to the _do-while_ loop, the following will not print anything, because the condition evaluates to `false` at the beginning of the first iteration:

    int i =0;
    while (i < 0)
    {
        std::cout << i;
        ++i; // Increment counter
    }    
    std::cout << std::endl; // End of line; nothing is printed to the console

Note: A _while_ loop can be exited without the condition becoming false by using a `break`, `goto`, or `return` statement. 

    int i = 0;
    do
    {
        std::cout << i;
        ++i; // Increment counter
        if (i > 5) 
        {
            break;
        }
    }
    while (true);
    std::cout << std::endl; // End of line; 0 1 2 3 4 5 is printed to the console

A trivial _do-while_ loop is also occasionally used to write macros that require their own scope (in which case the trailing semicolon is omitted from the macro definition and required to be provided by the user):

    #define BAD_MACRO(x) f1(x); f2(x); f3(x);

    // Only the call to f1 is protected by the condition here
    if (cond) BAD_MACRO(var);

    #define GOOD_MACRO(x) do { f1(x); f2(x); f3(x); } while(0)

    // All calls are protected here
    if (cond) GOOD_MACRO(var);

## Loop Control statements : Break and Continue
Loop control statements are used to change the flow of execution from its normal sequence. When execution leaves a scope, all automatic objects that were created in that scope are destroyed. The `break` and `continue` are loop control statements.

The `break` statement terminates a loop without any further consideration.

    for (int i = 0; i < 10; i++)
    {
        if (i == 4)
            break; // this will immediately exit our loop
        std::cout << i << '\n';
    }
The above code will print out:

    1
    2
    3

The `continue` statement does not immediately exit the loop, but rather skips the rest of the loop body and goes to the top of the loop (including checking the condition).

    for (int i = 0; i < 6; i++)
    {
        if (i % 2 == 0) // evaluates to true if i is even
            continue; // this will immediately go back to the start of the loop
        /* the next line will only be reached if the above "continue" statement 
           does not execute  */
        std::cout << i << " is an odd number\n";
    }

The above code will print out:

    1 is an odd number
    3 is an odd number
    5 is an odd number

Because such control flow changes are sometimes difficult for humans to easily understand, `break` and `continue` are used sparingly.  More straightforward implementation are usually easier to read and understand.  For example, the first `for` loop with the `break` above might be rewritten as:

    for (int i = 0; i < 4; i++)
    {
        std::cout << i << '\n';
    }

The second example with `continue` might be rewritten as:

    for (int i = 0; i < 6; i++)
    {
        if (i % 2 != 0) {
            std::cout << i << " is an odd number\n";
        }
    }


  [1]: http://en.cppreference.com/w/cpp/language/break
  [2]: http://en.cppreference.com/w/cpp/language/continue



## Declaration of variables in conditions
In the condition of the `for` and `while` loops, it's also permitted to declare an object.  This object will be considered to be in scope until the end of the loop, and will persist through each iteration of the loop:

    for (int i = 0; i < 5; ++i) {
        do_something(i);
    }
    // i is no longer in scope.

    for (auto& a : some_container) {
        a.do_something();
    }
    // a is no longer in scope.

    while(std::shared_ptr<Object> p = get_object()) {
       p->do_something();
    }
    // p is no longer in scope.

However, it is not permitted to do the same with a `do...while` loop; instead, declare the variable before the loop, and (optionally) enclose both the variable and the loop within a local scope if you want the variable to go out of scope after the loop ends:

    //This doesn't compile
    do {
        s = do_something();
    } while (short s > 0);

    // Good
    short s;
    do {
        s = do_something();
    } while (s > 0);

This is because the _statement_ portion of a `do...while` loop (the loop's body) is evaluated before the _expression_ portion (the `while`) is reached, and thus, any declaration in the _expression_ will not be visible during the first iteration of the loop.

## Range-for over a sub-range
Using range-base loops, you can loop over a sub-part of a given container or other range by generating a proxy object that qualifies for range-based for loops.

    template<class Iterator, class Sentinel=Iterator>
    struct range_t {
      Iterator b;
      Sentinel e;
      Iterator begin() const { return b; }
      Sentinel end() const { return e; }
      bool empty() const { return begin()==end(); }
      range_t without_front( std::size_t count=1 ) const {
        if (std::is_same< std::random_access_iterator_tag, typename std::iterator_traits<Iterator>::iterator_category >{} ) {
          count = (std::min)(std::size_t(std::distance(b,e)), count);
        }
        return {std::next(b, count), e};
      }
      range_t without_back( std::size_t count=1 ) const {
        if (std::is_same< std::random_access_iterator_tag, typename std::iterator_traits<Iterator>::iterator_category >{} ) {
          count = (std::min)(std::size_t(std::distance(b,e)), count);
        }
        return {b, std::prev(e, count)};
      }
    };

    template<class Iterator, class Sentinel>
    range_t<Iterator, Sentinel> range( Iterator b, Sentinal e ) {
      return {b,e};
    }
    template<class Iterable>
    auto range( Iterable& r ) {
      using std::begin; using std::end;
      return range(begin(r),end(r));
    }

    template<class C>
    auto except_first( C& c ) {
      auto r = range(c);
      if (r.empty()) return r;
      return r.without_front();
    }
now we can do:

    std::vector<int> v = {1,2,3,4};

    for (auto i : except_first(v))
      std::cout << i << '\n';
and print out

    2
    3
    4
Be aware that intermediate objects generated in the `for(:range_expression)` part of the `for` loop will have expired by the time the `for` loop starts.



