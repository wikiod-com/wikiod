---
title: "Parameterized Types"
slug: "parameterized-types"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

All composite types other than arrays can have discriminants, which are components with special properties. Discriminants can be of a discrete type or an access type. In the latter case the access type can be a named access type or it can be anonymous. A discriminant of an anonymous access type is called an access discriminant by analogy with an access parameter.

## Discriminated record types
In the case of a discriminated record type, some of the components are known
as discriminants and the remaining components can depend upon these. The
discriminants can be thought of as parameterizing the type and the syntax reveals
this analogy.
In this example we create a type that provide a square matrix with a positive as parameter :

    type Square(X: Positive) is
        record
            S: Matrix(1 .. X, 1 .. X);
        end record;

Then to create a square of 3 by 3, just call yout type Square like this :

    Sq: Square(3);

## Variant Record Structures
A discriminant of a record type may influence the structure of objects. A choice of components may exists in an object according as a discriminant had had a particular value when the object was created. To support this variation, a record type's definition includes a distinction by cases that depends on the discriminant:

    type Fruit is (Banana, Orange, Pear);
    
    type Basket (Kind : Fruit) is
       record
          case Kind is
             when Banana =>
                Bunch_Size      : Positive;
                Bunches_Per_Box : Natural;
             when Pear | Orange =>
                Fruits_Per_Box  : Natural;
          end case;
       end record;

Then to create a box for bananas,

    Box : Basket (Banana);

The `Box` object now has two record components in addition to its discriminant, `Kind`, namely `Bunch_Size` and `Bunches_Per_Box`.

