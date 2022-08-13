---
title: "Destructuring Assignment"
slug: "destructuring-assignment"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Named Function Parameters
CoffeeScript allows to deconstruct objects and arrays when they are fed to functions as arguments.

A function that leverages deconstruction will specify in its signature all the fields that are expected within its body. When invoking such function, an object or array containing all the expected fields has to be passed as argument.

```
drawRect = ({x, y, width, height}) ->
  # here you can use the passed parameters
  # color will not be visible here!

myRectangle = 
  x: 10
  y: 10
  width: 20
  height: 20
  color: 'blue'

drawRect myRectangle
```

```
printTopThree = ([first, second, third]) ->
  # here you can use the passed parameters
  # 'Scrooge McDuck' will not be visible here!

ranking = ['Huey', 'Dewey', 'Louie', 'Scrooge McDuck']

printTopThree ranking
```

## Swap
When you assign an array or object literal to a value, CoffeeScript breaks up and matches both sides against each other, assigning the values on the right to the variables on the left.

    # Swap
    [x, y] = [y, x]

## Extract Values from an Object
    person =
      name: "Duder von Broheim"
      age: 27
      address: "123 Fake St"
      phoneNumber: "867-5309"

    {name, age, address, phoneNumber} = person

## First and Last Element
    array = [1, 2, 3, 4]

    [first] = array # 1

    [..., last] = array # 4

    [first, middle..., last] = array # first is 1, middle is [2, 3], last is 4 

