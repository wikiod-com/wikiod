---
title: "Enumeration"
slug: "enumeration"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- function Enumeration'[Image][1] (Argument : Enumeration'Base) return String;
- function Enumeration'[Img][2] return String; -- GNAT
- function Enumeration'Val (Argument : Universal_Integer) return Enumeration'Base;
- function Enumeration'Pos (Argument : Enumeration'Base) return Universal_Integer;
- function Enumeration'Enum_Rep (Argument : Enumeration'Base) return Universal_Integer;
- function __Literal__'Enum_Rep return Universal_Integer; -- GNAT
- function __Literal__'Address return System.Address;
- for Enumeration use (Literal_1 => __Universal_Integer__, Literal_n => __Universal_Integer__);
- (__Literal__ in Enumeration) return Boolean;


  [1]: https://www.wikiod.com/ada/attribute-image
  [2]: https://www.wikiod.com/ada

## Iterating literals
A literal inside a enumeration is a discrete type so we can use attribute [`Image`][1]  to find out which literal it is as text form. Notice that this prints out the same word as in the code (but in upper case).

    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Main is
       type Fruit is (Banana, Pear, Orange, Melon);
    begin
       for I in Fruit loop
          Put (Fruit'Image (I));
          New_Line;
       end loop;
    end;

# Result

    BANANA
    PEAR
    ORANGE
    MELON

  [1]: https://www.wikiod.com/ada/attribute-image

## Using package Enumeration_IO
Instead of attribute [`Image`][1] and [`Ada.Text_IO.Put`][3] on enumeration literals we can only use the generic package [`Ada.Text_IO.Enumeration_IO`][4] to print out the literals.

    with Ada.Text_IO; use Ada.Text_IO;
    
    procedure Main is
       type Fruit is (Banana, Pear, Orange, Melon);
       package Fruit_IO is new Enumeration_IO (Fruit); use Fruit_IO;
    begin
       for I in Fruit loop
          Put (I);
          New_Line;
       end loop;
    end;
# Result

    BANANA
    PEAR
    ORANGE
    MELON

  [1]: https://www.wikiod.com/ada/attribute-image
  [2]: https://www.wikiod.com/ada
  [3]: http://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-10-1.html
  [4]: http://www.adaic.org/resources/add_content/standards/05aarm/html/AA-A-10-10.html

## First character upper case rest lower case literals
Attribute [`Image`][1] capitalizes all characters of enumeration literals. The function `Case_Rule_For_Names` applies upper case for the first character and makes the rest lower case.

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
    with Ada.Strings.Fixed; use Ada.Strings.Fixed;
    
    procedure Main is
       type Fruit is (Banana, Pear, Orange, Melon);
       function Case_Rule_For_Names (Item : String) return String is
       begin
          return Translate (Item (Item'First .. Item'First), Upper_Case_Map) & Translate (Item (Item'First + 1 .. Item'Last), Lower_Case_Map);
       end;
    begin
       for I in Fruit loop
          Put (Case_Rule_For_Names (Fruit'Image (I)));
          New_Line;
       end loop;
    end;

# Result

    Banana
    Pear
    Orange
    Melon


  [1]: https://www.wikiod.com/ada/attribute-image

## Title Case, Using Enumeration_IO, For a Subrange
Combining [change of character case][.enumC] with [Enumeration_IO][.enumIO] and using a text buffer for the image. The first character is manipulated in place.

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Characters.Handling; use Ada.Characters.Handling;

    procedure Main is
        type Fruit is (Banana, Pear, Orange, Melon);
        package Fruit_IO is new Enumeration_IO (Fruit);
        Buffer : String (1 .. Fruit'Width);
    begin
       for I in Fruit range Pear .. Fruit'Last loop
           Fruit_IO.Put (To => Buffer,
                         Item => I,
                         Set => Lower_Case);
           Buffer (Buffer'First) := To_Upper (Buffer (Buffer'First));
           Put_Line (Buffer);
       end loop;
    end;

# Result

    Pear  
    Orange
    Melon 

  [.enumC]: https://www.wikiod.com/ada/enumeration#First character upper case rest lower case literals 
  [.enumIO]: https://www.wikiod.com/ada/enumeration#Using package Enumeration_IO

