---
title: "Recursion"
slug: "recursion"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Tail Recursion
Using regular recursion, each recursive call pushes another entry onto the call stack. When the recursion is completed, the application has to pop each entry off all the way back down. If there are much recursive function calls it can end up with a huge stack.

Scala automatically removes the recursion in case it finds the recursive call in tail position. The annotation (``@tailrec``) can be added to recursive functions to ensure that tail call optimization is performed. The compiler then shows an error message if it can't optimize your recursion.

## Regular Recursion ##

This example is not tail recursive because when the recursive call is made, the function needs to keep track of the multiplication it needs to do with the result after the call returns.

```
def fact(i : Int) : Int = {
   if(i <= 1) i
   else i * fact(i-1)
}

println(fact(5))
```

The function call with the parameter will result in a stack that looks like this:

```
(fact 5)
(* 5 (fact 4))
(* 5 (* 4 (fact 3)))
(* 5 (* 4 (* 3 (fact 2))))
(* 5 (* 4 (* 3 (* 2 (fact 1)))))
(* 5 (* 4 (* 3 (* 2 (* 1 (fact 0))))))
(* 5 (* 4 (* 3 (* 2 (* 1 * 1)))))
(* 5 (* 4 (* 3 (* 2))))
(* 5 (* 4 (* 6)))
(* 5 (* 24))
120
```

If we try to annotate this example with ``@tailrec`` we will get the following error message: ``could not optimize @tailrec annotated method fact: it contains a recursive call not in tail position``

## Tail Recursion ##

In tail recursion, you perform your calculations first, and then you execute the recursive call, passing the results of your current step to the next recursive step.

```    
def fact_with_tailrec(i : Int) : Long = {
   @tailrec
   def fact_inside(i : Int, sum: Long) : Long = {
      if(i <= 1) sum
      else fact_inside(i-1,sum*i)
   }
   fact_inside(i,1)
}

println(fact_with_tailrec(5))
```

In contrast, the stack trace for the tail recursive factorial looks like the following:
```
(fact_with_tailrec 5)
(fact_inside 5 1)
(fact_inside 4 5)
(fact_inside 3 20)
(fact_inside 2 60)
(fact_inside 1 120)
```

There is only the need to keep track of the same amount of data for every call to ``fact_inside`` because the function is simply returning the value it got right through to the top. This means that even if ``fact_with_tail 1000000`` is called, it needs only the same amount of space as ``fact_with_tail 3``. This is not the case with the non-tail-recursive call, and as such large values may cause a stack overflow.

## Stackless recursion with trampoline(scala.util.control.TailCalls)
It is very common to get a `StackOverflowError` error while calling recursive function. Scala standard library offers [TailCall](http://www.scala-lang.org/api/current/scala/util/control/TailCalls$.html) to avoid stack overflow by using heap objects and continuations to store the local state of the recursion.

Two examples from the [scaladoc of TailCalls](http://www.scala-lang.org/api/current/scala/util/control/TailCalls$.html)
 
    import scala.util.control.TailCalls._
        
    def isEven(xs: List[Int]): TailRec[Boolean] =
      if (xs.isEmpty) done(true) else tailcall(isOdd(xs.tail))
        
    def isOdd(xs: List[Int]): TailRec[Boolean] =
      if (xs.isEmpty) done(false) else tailcall(isEven(xs.tail))
    
    // Does this List contain an even number of elements?
    isEven((1 to 100000).toList).result

    def fib(n: Int): TailRec[Int] =
      if (n < 2) done(n) else for {
        x <- tailcall(fib(n - 1))
        y <- tailcall(fib(n - 2))
      } yield (x + y)
    
    // What is the 40th entry of the Fibonacci series?
    fib(40).result

