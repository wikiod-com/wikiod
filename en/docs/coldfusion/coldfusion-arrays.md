---
title: "ColdFusion Arrays"
slug: "coldfusion-arrays"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
 - ArrayNew(dimension, isSynchronized)


## Parameters
| Name| Description |
| ------ | ------ |
| Dimension | Number of dimensions in new array: 1, 2, or 3 |
| isSynchronized| When *false*, creates an unsynchronized array,  When *true*, the function returns a synchronized array.|

In a synchronized array, more than two threads cannot access the array simultaneously. Other threads has to wait until the active thread completes its job, resulting in significant performance.

In 2016 ColdFusion release, you can use an unsynchronized array and let multiple threads access the same array object simultaneously.

## Creating Arrays
*Creating arrays explicitly using ArrayNew()*
------------------------------------------------------

Declare an array with the ArrayNew function. Specify the number of dimensions as an argument. 

 - ArrayNew(*dimension*) creates an array of 1–3 dimensions.
 - ColdFusion arrays expand dynamically as data is added.
 - ArrayNew() returns an array.

*History*
---------

Introduced in ColdFusion MX 6

*Declaration*
-------------

CFML

    <!--- One Dimension Array--->
    <cfset oneDimensionArray = ArrayNew(1)>

CFScript
Note that inside a function you should use `var` scoping. Earlier versions of CF required var scoping to be the first thing in a function; later versions allow it anywhere in a function.

    <cfscript>
        oneDimensionArray = ArrayNew(1);
    
        public void function myFunc() { 
            var oneDimensionArray = ArrayNew(1);
        } 
    </cfscript>

After creating the array, add elements by using the element indexes. The Coldfusion Array index starts from 1:

CFML

    <cfset oneDimensionArray[1] = 1>
    <cfset oneDimensionArray[2] = 'one'>
    <cfset oneDimensionArray[3] = '1'>

CFScript

    <cfscript>
        oneDimensionArray[1] = 1;
        oneDimensionArray[2] = 'one';
        oneDimensionArray[3] = '1';
    </cfscript>

**Using ArrayAppend()**
-----------------------

You can add elements to an array using the function `ArrayAppend(array, value)`.

    <cfscript>
        ArrayAppend(oneDimensionArray, 1);
        ArrayAppend(oneDimensionArray, 'one');
        ArrayAppend(oneDimensionArray, '1');
    </cfscript>

Output the array contents using `<cfdump>`:

    <cfdump var="#oneDimensionArray#">

Results:

[![one Dimension Array Dump Result][1]][1]

CFML

    <!--- Two Dimension Array--->
    <cfset twoDimensionArray = ArrayNew(2)>
    <cfset twoDimensionArray[1][1] = 1>
    <cfset twoDimensionArray[1][2] = 2>
    <cfset twoDimensionArray[2][1] = 3>
    <cfset twoDimensionArray[2][2] = 4>

CFScript

    <cfscript>
        twoDimensionArray = ArrayNew(2);
        
        twoDimensionArray[1][1] = 1;
        twoDimensionArray[1][2] = 2;
        twoDimensionArray[2][1] = 3;
        twoDimensionArray[2][2] = 4;
    </cfscript>

Outputting the contents of array using `<cfdump>`

    <cfdump var="#twoDimensionArray#">

Result:

[![Two Dimension Array Dump Result][2]][2]

Each element contains another Array, which will store the values.


*Creating 1-D Array Implicitly*
-------------------------------

When creating an array implicitly, brackets ([]) surround the array contents with comma separators.

    <cfset oneDimensionArrayImplicit = [ 1 ,'one','1' ]>

This statement is equivalent to the four statements used to create the above oneDimensionArray. The result are the same when using:

    <cfdump var="#oneDimensionArrayImplicit#">

*Create 2-D Array Implicitly*
-------------------------------


    <cfset twoDimensionArrayImplicit = [[ 1 , 2 ],[ 3 , 4 ],[ 5 , 6 ]]>

Or:

    <cfset firstElement = ["1", "2"]> 
    <cfset secondElement= ["3", "4"]> 
    <cfset thirdElement = ["5", "6"]> 
    <cfset twoDimensionArrayImplicit = [firstElement , secondElement, thirdElement]>

Outputting the content using 

    <cfdump var="#twoDimensionArrayImplicit#">

[![Implicit two Dimension Array][3]][3]

**Alternative Explicit Declaration**

Also you can declare 1 Dimension Array as

    <cfset oneDimensionArray = []>

    <cfscript>
        oneDimensionArray = [];
    </cfscript>

This declaration is same as that of using `ArrayNew(1)`.

But if you try declaring 2 Dimension Array as

    <cfset twoDimensionArray =[][]>    //Invalid CFML construct

an error will occur while processing this request.

Following statement will process the request: 

    <cfset twoDimensionArray =[]>

but variable `twoDimensionArray` will not actually an Array within Array (or 2-Dimension Array). It actually contains structure within Array.

    <cfset twoDimensionArray =[]>
    <cfset twoDimensionArray[1][1] = 1>
    <cfset twoDimensionArray[1][2] = 2>
    <cfset twoDimensionArray[2][1] = 3>
    <cfset twoDimensionArray[2][2] = 4>
    
    <cfdump var="#twoDimensionArray#">

Result:

[![two Dimension Array using [] explicit declaration][4]][4]


  [1]: http://i.stack.imgur.com/PovkZ.png
  [2]: http://i.stack.imgur.com/G2zEe.png
  [3]: http://i.stack.imgur.com/TvrzE.png
  [4]: http://i.stack.imgur.com/6nHIJ.png

## Array in CFScript
    <cfscript>
        oneDimensionArray = ArrayNew(1);
        oneDimensionArray[1] = 1;
        oneDimensionArray[2] = 'one';
        oneDimensionArray[3] = '1';
    </cfscript>
    
    <cfif IsDefined("oneDimensionArray")>
        <cfdump var="#oneDimensionArray#">
    </cfif>

Result:

[![Array in CFSCRIPT][1]][1]

Also, we can declare an one Dimension Array as:

    oneDimensionArray = [];


Alternatively, CF introduced `WriteDump()` from **CF9** as a function equivalent to the `<cfdump>` tag which can be used in `<cfscript>`.

    <cfscript>
        WriteDump(oneDimensionArray);
    </cfscript>

*Similarly, for 2 Dimension Array:*
-----------------------------------

    <cfscript>
        twoDimensionArray = ArrayNew(2);
        twoDimensionArray[1][1] = 1;
        twoDimensionArray[1][2] = 2;
        twoDimensionArray[2][1] = 3;
        twoDimensionArray[2][2] = 4;
    </cfscript>
    <cfdump var="#twoDimensionArray#">

Result:

[![Two Dimension Array in CFScript][2]][2]


  [1]: http://i.stack.imgur.com/D9Rtr.png
  [2]: http://i.stack.imgur.com/0BOoB.png

## General information
First some general information about how arrays behave in Coldfusion as compared to other programming languages.

* Arrays can have numeric indexes only (if you want to have a string index use `struct`s instead)
* Arrays begin at index [1]
* Arrays can have one ore more _dimensions_


