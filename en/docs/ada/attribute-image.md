---
title: "Attribute Image"
slug: "attribute-image"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

Subtype attributes `'Image` and `'Value` will take, respectively, a scalar value and a string and they return, respectively, a string and a scalar value. The result of `'Image` can be input to `'Value` to get the original value. The converse is also true.


The `__Scalar_Object__'Image` attribute can be used directly on objects (since Ada 2012-TC-1).


## Syntax
- function Scalar'Image (Argument : Scalar'Base) return String;
- function Discrete'Image (Argument  : Discrete'Base) return String;
- function Integer'Image (Argument : Integer'Base) return String;
- function Enumeration'Image (Argument : Enumeration'Base) return String;
- function Real'Image (Argument : Real'Base) return String;
- function Numeric'Image (Argument : Numeric'Base) return String;
- function Scalar'Value (Argument : String) return Scalar'Base;
- function Discrete'Value (Argument : String) return Discrete'Base;
- function Integer'Value (Argument : String) return Integer'Base;
- function Enumeration'Value (Argument : String) return Enumeration'Base;
- function Real'Value (Argument : String) return Real'Base;
- function __Scalar_Object__'Image return String;


Note that `'Image` can incur implementation defined results (RM 3.5), namely when some graphic characters needed for the `String` result are not defined in `Character`. Consider the larger repertoires of `'Wide_Image` and `'Wide_Wide_Image`.

<!-- if version [eq Ada 2012(TC-1)] -->
The permission to use the attribute `__Scalar_Object__'Image` directly on an object was added in Ada 2012-TC-1 (April 2016).
<!-- end version if -->

## Print out float using the Image attribute
<!-- if version [eq Ada 2012(TC-1)] -->
    with Ada.Text_IO;
    
    procedure Main is
       type Some_Float digits 8 range 0.0 .. 10.0;
       X : Some_Float := 2.71;
    begin
       Ada.Text_IO.Put_Line (X'Image);
    end Main;
<!-- end version if -->
# Result

    2.71000E+00

## Print out integer using the Image attribute
<!-- if version [eq Ada 2012(TC-1)] -->

    with Ada.Text_IO;
    
    procedure Main is
       type Some_Integer is range -42 .. 42;
       X : Some_Integer := 17;
    begin
       Ada.Text_IO.Put_Line (X'Image);
    end Main;
<!-- end version if -->
# Result

    17

## Print out enumeration using the Image attribute
<!-- if version [eq Ada 2012(TC-1)] -->
    with Ada.Text_IO;
    
    procedure Main is
       type Fruit is (Banana, Orange, Pear);
       X : Fruit := Orange;
    begin
       Ada.Text_IO.Put_Line (X'Image);
       Ada.Text_IO.Put_Line (Pear'Image);
    end Main;
<!-- end version if -->
# Result

    ORANGE
    PEAR

## Print out Enumeration using attribute Image
    with Ada.Text_IO;
    
    procedure Main is
       type Fruit is (Banana, Orange, Pear);
       X : Fruit := Orange;
    begin
       Ada.Text_IO.Put_Line (Fruit'Image (X));
    end Main;

# Result

    ORANGE

## Print out Integer using attribute Image
    with Ada.Text_IO;
    
    procedure Main is
       X : Integer := 17;
    begin
       Ada.Text_IO.Put_Line (Integer'Image (X));
    end Main;

# Result

    17

## Print out Float using attribute Image
    with Ada.Text_IO;
    
    procedure Main is
       X : Float := 2.71;
    begin
       Ada.Text_IO.Put_Line (Float'Image (X));
    end Main;

# Result

    2.71000E+00

## As Inverses
<!-- language: lang-ada -->
    with Ada.Text_IO;

    procedure Image_And_Value is
       type Fruit is (Banana, Orange, Pear);
       X  : Fruit := Orange;
    begin
       Ada.Text_IO.Put_Line (Boolean'Image
          (Fruit'Value (Fruit'Image (X)) = X
              and
           Fruit'Image (Fruit'Value ("ORANGE")) = "ORANGE"));
    end Image_And_Value;

# Result

    TRUE



