---
title: "Operator Overloading"
slug: "operator-overloading"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Defining Custom Infix Operators
In Scala operators (such as `+`, `-`, `*`, `++`, etc.) are just methods. For instance, `1 + 2` can be written as `1.+(2)`. These sorts of methods are called [*'infix operators'*][1].

This means custom methods can be defined on your own types, reusing these operators:

    class Matrix(rows: Int, cols: Int, val data: Seq[Seq[Int]]){
      def +(that: Matrix) = {
        val newData = for (r <- 0 until rows) yield
          for (c <- 0 until cols) yield this.data(r)(c) + that.data(r)(c)
    
        new Matrix(rows, cols, newData)
      }
    }

These operators defined-as-methods can be used like so:

    val a = new Matrix(2, 2, Seq(Seq(1,2), Seq(3,4)))
    val b = new Matrix(2, 2, Seq(Seq(1,2), Seq(3,4)))
    
    // could also be written a.+(b)
    val sum = a + b  

Note that infix operators can only have a single argument; the object before the operator will call it's own operator on the object after the operator. Any Scala method with a single argument can be used as an infix operator.

> This should be used with parcimony. It is generally considered good practice only if your own method does exactly what one would expect from that operator. In case of doubt, use a more conservative naming, like `add` instead of `+`. 


  [1]: http://docs.scala-lang.org/tutorials/tour/operators.html

## Defining Custom Unary Operators
Unary operators can be defined by prepending the operator with `unary_`. Unary operators are limited to `unary_+`, `unary_-`, `unary_!` and `unary_~`:

    class Matrix(rows: Int, cols: Int, val data: Seq[Seq[Int]]){
      def +(that: Matrix) = {
        val newData = for (r <- 0 until rows) yield
          for (c <- 0 until cols) yield this.data(r)(c) + that.data(r)(c)
    
        new Matrix(rows, cols, newData)
      }

      def unary_- = {
        val newData = for (r <- 0 until rows) yield 
          for (c <- 0 until cols) yield this.data(r)(c) * -1
       
        new Matrix(rows, cols, newData) 
      }   
    }

The unary operator can be used as follows:

    val a = new Matrix(2, 2, Seq(Seq(1,2), Seq(3,4)))
    val negA = -a
   
> This should be used with parcimony. Overloading a unary operator with a definition that is not what one would expect can lead to code confusion.

