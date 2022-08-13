---
title: "Expressions"
slug: "expressions"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Intro to Expressions
Expressions are a specific type of object in Julia.  You can think of an expression as representing a piece of Julia code that has not yet been evaluated (i.e. executed).  There are then specific functions and operations, like `eval()` which will evaluate the expression.

For instance, we could write a script or enter into the interpreter the following:
    julia> 1+1
    2

One way to create an expression is using the `:()` syntax.  For example:

    julia> MyExpression = :(1+1)
    :(1 + 1)
    julia> typeof(MyExpression)
    Expr

We now have an `Expr` type object.  Having just been formed, it doesn't do anything - it just sits around like any other object until it is acted upon.  In this case, we can *evaluate* that expression using the `eval()` function:

    julia> eval(MyExpression)
    2

Thus, we see that the following two are equivalent:

    1+1
    eval(:(1+1))

Why would we want to go through the much more complicated syntax in `eval(:(1+1))` if we just want to find what 1+1 equals?  The basic reason is that we can define an expression at one point in our code, potentially modify it later on, and then evaluate it at a later point still.  This can potentially open up powerful new capabilities to the Julia programmer.  Expressions are a key component of [metaprogramming](https://www.wikiod.com/julia-lang/metaprogramming) in Julia.

## Creating Expressions
There are a number of different methods that can be used to create the same type of expression.  The [expressions intro](https://www.wikiod.com/julia-lang/expressions#Intro to Expressions) mentioned the `:()` syntax.  Perhaps the best place to start, however is with strings.  This helps to reveal some of the fundamental similarities between expressions and strings in Julia.

**Create Expression from String**

From the Julia [documentation](http://docs.julialang.org/en/release-0.4/manual/metaprogramming/):

> Every Julia program starts life as a string

In other words, any Julia script is simply written in a text file, which is nothing but a string of characters.  Likewise, any Julia command entered into an interpreter is just a string of characters.  The role of Julia or any other programming language then is to interpret and evaluate strings of characters in a logical, predictable way so that those strings of characters can be used to describe what the programmer wants the computer to accomplish.  

Thus, one way to create an expression is to use the `parse()` function as applied to a string.  The following expression, once it is evaluated, will assign the value of 2 to the symbol `x`.

    MyStr = "x = 2"
    MyExpr = parse(MyStr)
    julia> x
    ERROR: UndefVarError: x not defined
    eval(MyExpr)
    julia> x
    2

**Create Expression Using `:()` Syntax**

    MyExpr2 = :(x = 2)
    julia> MyExpr == MyExpr2
    true

Note that with this syntax, Julia will automatically treat the names of objects as referring to symbols.  We can see this if we look at the `args` of the expression.  (See [Fields of Expression Objects](https://www.wikiod.com/julia-lang/expressions#Fields of Expression Objects) for more details on the `args` field in an expression.)

    julia> MyExpr2.args
    2-element Array{Any,1}:
      :x
     2  

**Create Expression using the `Expr()` Function**

    MyExpr3 = Expr(:(=), :x, 2)
    MyExpr3 == MyExpr

This syntax is based on [prefix notation](https://en.wikipedia.org/wiki/Polish_notation).  In other words, the first argument of the specified to the `Expr()` function is the `head` or prefix.  The remaining are the `arguments` of the expression.  The `head` determines what operations will be performed on the arguments.  

For more details on this, see [Fields of Expression Objects](https://www.wikiod.com/julia-lang/expressions#Fields of Expression Objects)

When using this syntax, it is important to distinguish between using objects and symbols for objects.  For instance, in the above example, the expression assigns the value of `2` to the symbol `:x`, a perfectly sensible operation.  If we used `x` itself in an expression such as that, we would get the nonsensical result:

    julia> Expr(:(=), x, 5)
    :(2 = 5)

Similarly, if we examine the `args` we see:

    julia> Expr(:(=), x, 5).args
    2-element Array{Any,1}:
     2
     5

Thus, the `Expr()` function does not perform the same automatic transformation into symbols as the `:()` syntax for creating expressions.

**Create multi-line Expressions using `quote...end`**

    MyQuote = 
    quote
        x = 2
        y = 3
    end
    julia> typeof(MyQuote)
    Expr

Note that with `quote...end` we can create expressions that contain other expressions in their `args` field:

    julia> typeof(MyQuote.args[2])
    Expr

See [Fields of Expression Objects](https://www.wikiod.com/julia-lang/expressions#Fields of Expression Objects) for more on this `args` field.

**More on Creating Expressions**

This Example just gives the basics for creating expressions.  See also, for example, [Interpolation and Expressions](https://www.wikiod.com/julia-lang/expressions#Interpolation and Expressions) and [Fields of Expression Objects](https://www.wikiod.com/julia-lang/expressions#Fields of Expression Objects) for more information on creating more complex and advanced expressions.


## Fields of Expression Objects
As mentioned in the [Intro to Expressions](https://www.wikiod.com/julia-lang/expressions#Intro to Expressions) expressions are a specific type of object in Julia.  As such, they have fields.  The two most used fields of an expression are its `head` and its `args`.  For instance, consider the expression 
    
    MyExpr3 = Expr(:(=), :x, 2)

discussed in [Creating Expressions](https://www.wikiod.com/julia-lang/expressions#Creating Expressions).  We can see the `head` and `args` as follows:

    julia> MyExpr3.head
    :(=)

    julia> MyExpr3.args
    2-element Array{Any,1}:
      :x
     2  

Expressions are based on [prefix notation](https://en.wikipedia.org/wiki/Polish_notation).  As such, the `head` generally specifies the operation that is to be performed on the `args`.  The head must be of Julia type `Symbol`. 

When an expression is to assign a value (when it gets evaluated), it will generally use a head of `:(=)`.  There are of course obvious variations to this that can be employed, e.g.:

    ex1 = Expr(:(+=), :x, 2)

**:call for expression heads**

Another common `head` for expressions is `:call`.  E.g.

    ex2 = Expr(:call, :(*), 2, 3)
    eval(ex2) ## 6

Following the conventions of prefix notation, operators are evaluated from left to right.  Thus, this expression here means that we will call the function that is specified on the first element of `args` on the subsequent elements.  We similarly could have:

    julia> ex2a = Expr(:call, :(-), 1, 2, 3)
    :(1 - 2 - 3)

Or other, potentially more interesting functions, e.g. 

    julia> ex2b = Expr(:call, :rand, 2,2)
    :(rand(2,2))

    julia> eval(ex2b)
    2x2 Array{Float64,2}:
     0.429397  0.164478
     0.104994  0.675745


**Automatic determination of `head` when using `:()` expression creation notation**

Note that `:call` is implicitly used as the head in certain constructions of expressions, e.g.

    julia> :(x + 2).head
    :call

Thus, with the `:()` syntax for creating expressions, Julia will seek to automatically determine the correct head to use.  Similarly:

    julia> :(x = 2).head
    :(=)

In fact, if you aren't certain what the right head to use for an expression that you are forming using, for instance, `Expr()` this can be a helpful tool to get tips and ideas for what to use.



## Interpolation and Expressions
[Creating Expressions](https://www.wikiod.com/julia-lang/expressions#Creating Expressions) mentions that expressions are closely related to strings.  As such, the principles of interpolation within strings are also relevant for Expressions.  For instance, in basic string interpolation, we can have something like:

    n = 2
    julia> MyString = "there are $n ducks"
    "there are 2 ducks"

We use the `$` sign to insert the value of `n` into the string.  We can use the same technique with expressions.  E.g.

    a = 2
    ex1 = :(x = 2*$a)  ##     :(x = 2 * 2)
    a = 3
    eval(ex1)
    x # 4

Contrast this this:

    a = 2
    ex2 = :(x = 2*a) # :(x = 2a)
    a = 3
    eval(ex2)
    x # 6

Thus, with the first example, we set in advance the value of `a` that will be used at the time that the expression is evaluated.  With the second example, however, the Julia compiler will only look to `a` to find its value *at the time of evaluation* for our expression.



## External References on Expressions
There are a number of useful web resources that can help further your knowledge of expressions in Julia.  These include:

 - [Julia Docs - Metaprogramming](http://docs.julialang.org/en/release-0.4/manual/metaprogramming/)
 - [Wikibooks - Julia Metaprogramming](https://en.wikibooks.org/wiki/Introducing_Julia/Metaprogramming)
 - [Julia’s macros, expressions, etc. for and by the confused, by Gray Calhoun](http://gray.clhn.org/dl/macros_etc.pdf)
 - [Month of Julia - Metaprogramming, by Andrew Collier](http://www.juliabloggers.com/monthofjulia-day-11-metaprogramming/)
 - [Symbolic Differentiation in Julia, by John Myles White](http://www.johnmyleswhite.com/notebook/2013/01/07/symbolic-differentiation-in-julia/)

SO Posts:

 - [What is a "symbol" in Julia? Answer by Stefan Karpinski](http://stackoverflow.com/questions/23480722/what-is-a-symbol-in-julia)
 - [Why does julia express this expression in this complex way?](http://stackoverflow.com/questions/33494019/why-does-julia-express-this-expression-in-this-complex-way)
 - [Explanation of Julia expression interpolation example](http://stackoverflow.com/questions/31147928/explanation-of-julia-expression-interpolation-example)



