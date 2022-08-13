---
title: "Currying"
slug: "currying"
draft: false
images: []
weight: 9890
type: docs
toc: true
---

## Syntax
 - aFunction(10)_ //Using '_' Tells the compiler that all the parameters in the rest of the parameter groups will be curried.
 - nArityFunction.curried //Converts an n-arity Function to an equivalent curried version
 - anotherFunction(x)(_: String)(z) // Currying an arbitrary parameter. It needs its type explicitly stated.



## When to use Currying
**[Currying][1]** *is the technique of translating the evaluation of a function that takes multiple arguments into evaluating a sequence of functions, each with a single argument*.

This is normally useful when for example:

 1. different arguments of a function are calculated **at different times**. *(Example 1)*
 2. different arguments of a function are calculated **by different tiers of the application**. *(Example 2)*

**Example 1**

Let's assume that the total yearly income is a function composed by the income and a bonus:

    val totalYearlyIncome:(Int,Int) => Int =  (income, bonus) => income + bonus

The curried version of the above 2-arity function is:

    val totalYearlyIncomeCurried: Int => Int => Int = totalYearlyIncome.curried

Note in the above definition that the type can be also viewed/written as:

    Int => (Int => Int)

Let's assume that the yearly income portion is known in advance:

    val partialTotalYearlyIncome: Int => Int = totalYearlyIncomeCurried(10000)

And at some point down the line the bonus is known:

    partialTotalYearlyIncome(100)

**Example 2**

Let's assume that the car manufacturing involves the application of car wheels and car body:

    val carManufacturing:(String,String) => String = (wheels, body) => wheels + body

These parts are applied by different factories:

    class CarWheelsFactory {
      def applyCarWheels(carManufacturing:(String,String) => String): String => String =
              carManufacturing.curried("applied wheels..")
    }
        
    class CarBodyFactory {
      def applyCarBody(partialCarWithWheels: String => String): String = partialCarWithWheels("applied car body..")
    }

Notice that the `CarWheelsFactory` above curries the car manufacturing function and only applies the wheels.

The car manufacturing process then will take the below form:

    val carWheelsFactory = new CarWheelsFactory()
    val carBodyFactory   = new CarBodyFactory()
    
    val carManufacturing:(String,String) => String = (wheels, body) => wheels + body
      
    val partialCarWheelsApplied: String => String  = carWheelsFactory.applyCarWheels(carManufacturing)
    val carCompleted = carBodyFactory.applyCarBody(partialCarWheelsApplied)

  [1]: https://en.wikipedia.org/wiki/Currying

## Currying a function with a single parameter group
    def minus(left: Int, right: Int) = left - right

    val numberMinus5 = minus(_: Int, 5)
    val fiveMinusNumber = minus(5, _: Int)

    numberMinus5(7)    //  2
    fiveMinusNumber(7) // -2

## Currying
Currying, according [to Wikipedia](https://en.wikipedia.org/wiki/Currying), 

> is the technique of translating the evaluation of a function that
> takes multiple arguments into evaluating a sequence of functions.

Concretely, in terms of scala types, in the context of a function that take two arguments, (has arity 2) it is the conversion of 
```
val f: (A, B) => C // a function that takes two arguments of type `A` and `B` respectively 
                   // and returns a value of type `C`
```
to 
```
val curriedF: A => B => C // a function that take an argument of type `A` 
                          // and returns *a function* 
                          // that takes an argument of type `B` and returns a `C`
```

So for arity-2 functions we can write the curry function as:

```
def curry[A, B, C](f: (A, B) => C): A => B => C = { 
  (a: A) => (b: B) => f(a, b) 
}
```
Usage:
```
val f: (String, Int) => Double = {(_, _) => 1.0}
val curriedF: String => Int => Double = curry(f)
f("a", 1)        // => 1.0
curriedF("a")(1) // => 1.0
```

Scala gives us a few language features that help with this:

1. You can write curried functions as methods. so `curriedF` can be written as:

```
def curriedFAsAMethod(str: String)(int: Int): Double = 1.0
val curriedF = curriedFAsAMethod _
```

2. You can un-curry (i.e. go from `A => B => C` to `(A, B) => C`) using a standard library method: `Function.uncurried`

```
val f: (String, Int) => Double = Function.uncurried(curriedF)
f("a", 1) // => 1.0
```


## A configurable multiplier as a curried function
    def multiply(factor: Int)(numberToBeMultiplied: Int): Int = factor * numberToBeMultiplied
    
    val multiplyBy3 = multiply(3)_     // resulting function signature Int => Int
    val multiplyBy10 = multiply(10)_ // resulting function signature Int => Int
    
    val sixFromCurriedCall = multiplyBy3(2) //6
    val sixFromFullCall = multiply(3)(2)    //6
    
    val fortyFromCurriedCall = multiplyBy10(4) //40
    val fortyFromFullCall = multiply(10)(4)    //40

## Currying
Let's define a function of 2 arguments:
```scala
def add: (Int, Int) => Int = (x,y) => x + y
val three = add(1,2) 
```
Currying `add` transforms it into a function that takes **one** `Int` and returns a **function** (from **one** `Int` to an `Int`)
```scala
val addCurried: (Int) => (Int => Int) = add2.curried
//               ^~~ take *one* Int
//                        ^~~~ return a *function* from Int to Int

val add1: Int => Int = addCurried(1)
val three: Int = add1(2)
val allInOneGo: Int = addCurried(1)(2)
```

You can apply this concept to any function that takes multiple arguments. Currying a function that takes multiple arguments, transforms it into a series of applications of functions that take **one** argument:
```scala
def add3: (Int, Int, Int) => Int = (a,b,c) => a + b + c + d
def add3Curr: Int => (Int => (Int => Int)) = add3.curried

val x = add3Curr(1)(2)(42)

## A real world use of Currying.
What we have is a list of credit cards and we'd like to calculate the premiums for all those cards that the credit card company has to pay out.
The premiums themselves depend on the total number of credit cards, so that the company adjust them accordingly.

We already have a function that calculates the premium for a single credit card and takes into account the total cards the company has issued:


    case class CreditCard(creditInfo: CreditCardInfo, issuer: Person, account: Account)

    object CreditCard {
      def getPremium(totalCards: Int, creditCard: CreditCard): Double = { ... }
    }



Now a reasonable approach to this problem would be to map each credit card to a premium and reduce it to a sum.
Something like this:

    val creditCards: List[CreditCard] = getCreditCards()
    val allPremiums = creditCards.map(CreditCard.getPremium).sum //type mismatch; found : (Int, CreditCard) ⇒ Double required: CreditCard ⇒ ?


However the compiler isn't going to like this, because `CreditCard.getPremium` requires two parameters.
Partial application to the rescue! We can partially apply the total number of credit cards and use that function to map the credit cards to their premiums. 
All we need to do is curry the `getPremium` function by changing it to use multiple parameter lists and we're good to go.

The result should look something like this:

    object CreditCard {
      def getPremium(totalCards: Int)(creditCard: CreditCard): Double = { ... }
    }

    val creditCards: List[CreditCard] = getCreditCards()

    val getPremiumWithTotal = CreditCard.getPremium(creditCards.length)_

    val allPremiums = creditCards.map(getPremiumWithTotal).sum


## Multiple parameter groups of different types, currying parameters of arbitrary positions
    def numberOrCharacterSwitch(toggleNumber: Boolean)(number: Int)(character: Char): String = 
      if (toggleNumber) number.toString else character.toString

    // need to explicitly specify the type of the parameter to be curried
    // resulting function signature Boolean => String
    val switchBetween3AndE = numberOrCharacterSwitch(_: Boolean)(3)('E') 

    switchBetween3AndE(true) // "3"
    switchBetween3AndE(false) // "E"

