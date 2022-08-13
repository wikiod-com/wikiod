---
title: "Genericity in Ada"
slug: "genericity-in-ada"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Generic Subprograms
Generic subprograms are usefull to create a subprograms that have the same structure for several types. For example, to swap two objects:

    generic
        type A_Type is private;
    procedure Swap (Left, Right : in out A_Type) is
        Temp : A_Type := Left;
    begin
        Left := Right;
        Right := Temp;
    end Swap;


## Generic Packages
In Ada generic package, upon instantiation, data are duplicated; that is, if they contain global variables, each instance will have its own copy of the variable, properly typed and independent from the others.

    generic
        type T is private;
    package Gen is
        type C is tagged record
            V : T;
        end record;
        G : Integer;
    end Gen;


## Generic Parameters
Ada offers a wide variety of generic parameters which is difficult to translate into other languages. The parameters used during instantiation and as a consequence those on which the generic unit may rely on may be variables, types, subprograms, or package instances, with certain properties. For example, the following provides a sort algorithm for any kind of array:

    generic
        type Component is private;
        type Index is (<>);
        with function "<" (Left, Right : Component) return Boolean;
        type Array_Type is array (Index range <>) of Component;
    procedure Sort (A : in out Array_Type);


