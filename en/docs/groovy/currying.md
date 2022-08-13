---
title: "Currying"
slug: "currying"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
- closure.curry(parameter)
- closure.rcurry(parameter)
- closure.ncurry(index, parameters ...)

* Currying a closure produces a new closure with one or more of it's parameters having a fixed value

* Left or right currying a closure that has no parameters or index based currying a closure that has less than two parameters throws an `IllegalArgumentException`


## Left currying
    def pow = { base, exponent ->
        base ** exponent
    }
    assert pow(3, 2) == 9

    def pow2 = pow.curry(2) //base == 2
    assert pow2(3) == 8


## Right currying
    def dividable = { a, b ->
        a % b == 0
    }
    assert dividable(2, 3) == false
    assert dividable(4, 2) == true

    def even = dividable.rcurry(2) // b == 2
    assert even(2) == true
    assert even(3) == false


## Index based currying
    def quatNorm = { a, b, c, d ->
        Math.sqrt(a*a + b*b + c*c + d*d)
    }
    assert quatNorm(1, 4, 4, -4) == 7.0

    def complexNorm = quatNorm.ncurry(1, 0, 0) // b, c == 0
    assert complexNorm(3, 4) == 5.0


## Currying closure with no explicit parameter
      def noParam = { 
          "I have $it"
      }

      def noParamCurry = noParam.curry(2)
      assert noParamCurry() == 'I have 2'

## Currying closure with no parameters
    def honestlyNoParam = { ->
        "I Don't have it"
    }

    // The following all throw IllegalArgumentException
    honestlyNoParam.curry('whatever')
    honestlyNoParam.rcurry('whatever')
    honestlyNoParam.ncurry(0, 'whatever')

