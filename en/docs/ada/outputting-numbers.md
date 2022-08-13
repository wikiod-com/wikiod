---
title: "Outputting numbers"
slug: "outputting-numbers"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Ada's standard packages provide for output of all numeric types. The format of output can be adjusted in many ways.

Note how each time a generic package is instantiated with a numeric type. Also, there are both defaults to be set for the whole instance, and also ways to override `Width`, say, when calling `Put` with this parameter.


## Print Integers, Using Base 16  (Hexadecimal)
A settings variable `Default_Base` is set on the instance of `Ada.Text_IO.Integer_IO`; also, `Default_Width` is set so that output cannot have leading space.

    with Ada.Text_IO;   use Ada.Text_IO;
    
    procedure Print_Hex is
        subtype Count is Integer range -1_000_000 .. 1_000_000;
    
        package Count_IO is new Integer_IO (Count);
        X : Count;
    begin
        Count_IO.Default_Width := 1;
        Count_IO.Default_Base := 16;
    
        X := Count'First;
        while X < Count'Last loop
            Count_IO.Put (X);
            New_Line;
    
            X := X + 500_000;
        end loop;
    end Print_Hex;

# Result

    -16#F4240#
    -16#7A120#
    16#0#
    16#7A120#

## Print integers, generously using space
Instances of `Integer_IO` have a settings variable `Default_Width` which the number of characters that each output number will take.

    with Ada.Text_IO;   use Ada.Text_IO;
    
    procedure Print_Integer is
        subtype Count is Integer range -1_000_000 .. 1_000_000;
    
        package Count_IO is new Integer_IO (Count);
        X : Count;
    begin
        Count_IO.Default_Width := 12;
    
        X := Count'First;
        while X < Count'Last loop
            Count_IO.Put (X);
            Count_IO.Put (X + 1);
            New_Line;
    
            X := X + 500_000;
        end loop;
    end Print_Integer;

# Result
        -1000000
         -500000
               0
          500000

## Print Decimal Fixed Point Numbers, aka Money
`Ada.Text_IO.Editing` offers formatting decimal fixed point values using “picture strings”. These describe output using “magical” characters for separators, currency signs, etc.

    with Ada.Text_IO.Editing;   use Ada.Text_IO;
    
    procedure Print_Value is
    
        Max_Count      : constant := 1_000_000;
    
        type Fruit is (Banana, Orange, Pear);
        subtype Count is Integer range -Max_Count .. +Max_Count;
    
        type Money is delta 0.001 digits 10;
    
        package Fruit_IO is new Enumeration_IO (Fruit);
        package Money_IO is new Editing.Decimal_Output
          (Money,
           Default_Currency => "CHF",
           Default_Separator => ''');
    
        Inventory : constant array (Fruit) of Count :=
          (Banana => +27_420,
           Orange => +140_600,
           Pear   => -10_000);
    
        Price_List : constant array (Fruit) of Money :=
          (Banana => 0.07,
           Orange => 0.085,
           Pear   => 0.21);
    
        Format : constant Editing.Picture :=
          Editing.To_Picture ("<###BZ_ZZZ_ZZ9.99>");
    begin
        Fruit_IO.Default_Width := 12;
    
        for F in Inventory'Range loop
            Fruit_IO.Put (F);
            Put          (" | ");
            Money_IO.Put (Item => Inventory (F) * Price_List (F),
                          Pic => Format);
            New_Line;
        end loop;
    end Print_Value;

# Result

    BANANA       |  CHF     1'919.40 
    ORANGE       |  CHF    11'951.00 
    PEAR         | (CHF     2'100.00)

## Print Multiple Items On One Line
Combine the instances of the `_IO` packages, use the right one with its numeric type.

    with Ada.Text_IO;   use Ada.Text_IO;
    
    procedure Print_Inventory is
        type Fruit is (Banana, Orange, Pear);
        subtype Count is Integer range -1_000_000 .. 1_000_000;
    
        package Fruit_IO is new Enumeration_IO (Fruit);
        package Count_IO is new Integer_IO (Count);
    
        Inventory : constant array (Fruit) of Count :=
          (Banana => 27_420,
           Orange => 140_600,
           Pear   => -10_000);
    
    begin
        Fruit_IO.Default_Width := 12;
    
        for F in Inventory'Range loop
            Fruit_IO.Put (F);
            Put          (" | ");
            Count_IO.Put (Inventory (F));
            New_Line;
        end loop;
    end Print_Inventory;

# Result

    BANANA       |    27420
    ORANGE       |   140600
    PEAR         |   -10000

