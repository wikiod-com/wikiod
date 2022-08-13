---
title: "For Loops"
slug: "for-loops"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Syntax
 - for OrdinalVariable := LowerOrdinalValue to UpperOrdinalValue do begin {loop-body} end;
 - for OrdinalVariable := UpperOrdinalValue downto LowerOrdinalValue do begin {loop-body} end;
 - for EnumerableVariable in Collection do begin {loop-body} end;

- Delphi's `for`-loop syntax does not provide anything to change step amount from `1` to any other value.
- When looping with variable ordinal values, e.g. local variables of type `Integer`, the upper and lower values will be determined only once. Changes to such variables will have no effect on the loops iteration count.

## Simple for loop
A `for` loop iterates from the starting value to the ending value inclusive.

    program SimpleForLoop;
    
    {$APPTYPE CONSOLE}
    
    var
      i : Integer;
    begin
      for i := 1 to 10 do
        WriteLn(i);
    end.

**Output:**
> 1<br/>
> 2<br/>
> 3<br/>
> 4<br/>
> 5<br/>
> 6<br/>
> 7<br/>
> 8<br/>
> 9<br/>
> 10<br/>

## Looping over characters of a string
<!-- if version [gte 2005] -->

The following iterates over the characters of the string `s`. It works similarly for looping over the elements of an array or a set, so long as the type of the loop-control variable (`c`, in this example) matches the element type of the value being iterated.

    program ForLoopOnString;
    
    {$APPTYPE CONSOLE}
    
    var
      s : string;
      c : Char;
    begin
      s := 'Example';
      for c in s do
        WriteLn(c);
    end.

**Output:**
> E<br/>
> x<br/>
> a<br/>
> m<br/>
> p<br/>
> l<br/>
> e

<!-- end version if -->

## Reverse-direction for loop
A `for` loop iterates from the starting value down to the ending value inclusive, as a "count-down" example.

    program CountDown;
    
    {$APPTYPE CONSOLE}
    
    var
      i : Integer;
    begin
      for i := 10 downto 0 do
        WriteLn(i);
    end.
    

**Output:**
> 10<br/>
> 9<br/>
> 8<br/>
> 7<br/>
> 6<br/>
> 5<br/>
> 4<br/>
> 3<br/>
> 2<br/>
> 1<br/>
> 0<br/>

## For loop using an enumeration
A `for` loop iterate through items in an enumeration

    program EnumLoop;
    
    uses
      TypInfo;
    
    type
      TWeekdays = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
    
    var
      wd : TWeekdays;
    begin
    
      for wd in TWeekdays do
        WriteLn(GetEnumName(TypeInfo(TWeekdays), Ord(wd)));
    
    end.


**Output:**
> Sunday<br/>
> Monday<br/>
> Tuesday<br/>
> Wednesday<br/>
> Thursday<br/>
> Friday<br/>
> Saturday<br/>

## For in array
A `for` loop iterate through items in an array

    program ArrayLoop;
    {$APPTYPE CONSOLE}  
    const a : array[1..3] of real = ( 1.1, 2.2, 3.3 );
    var f : real;
    begin
      for f in a do
        WriteLn( f );    
    end.

**Output:**
> 1,1<br/>
> 2,2<br/>
> 3,3

