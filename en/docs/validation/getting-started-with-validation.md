---
title: "Getting started with validation"
slug: "getting-started-with-validation"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting validation set up or installed.

## Validating a name entered by User
Validating the Name entered by a User contain the following check

 - Ensure it is not empty
 - Ensure it contain only alphabets, space and/or dot.

So, the regular expression for this is

    ^[A-Z][a-z]*(\.?\s?[A-Z][a-z]*)+$

This means

 - `^`  -> Should start with

 - `[A-Z]` -> the first letter should be capital case

 - `[a-z]*` -> the leading letters should be small case (Optional and not applicable for Initials. Ex : **J. Doe**)

 - `(\.?\s?[A-Z][a-z]*)+` -> A dot (.) and/or a space (" "), then a capital case and small cases. The last + indicates that this part can repeat many times and at least one time it should be there.

 - `$` end. No further words will be there

 > Example matching : J. Doe , John Doe, John Doe Doe, John D Doe.

<br /> 

**Example in JavaScript**
 
---

    var name = "John Doe";
    var noname = "123Abc";
    console.log(/^[A-Z][a-z]*(\.?\s?[A-Z][a-z]*)+$/.test(name)); // true
    console.log(/^[A-Z][a-z]*(\.?\s?[A-Z][a-z]*)+$/.test(noname)); // false



## Validating an age against a range
Let's take the Range is from 18 to 80.

So, to validate,

We should check that the age is a positive integer.

Then check it should be greater than or equal to 18 and less than or equal to 80.

The test to check whether it is a number or not can be performed by a simple regular expression like


    ^[0-9]+$
Or even simpler


    ^\d+$ 


Then we can check it with range as 


    if age>=18 && age <=80 return true
    else return false


## Validating whether the user input contain non alpha numeric character
Sometimes, we have to take input from users which should contain only alpha numeric characters.

For example, lets say a Username system which allow only letters and numbers,

Then this can be done with the following Regular Expression


    ^[a-zA-Z0-9]+$

 - `^` is restrict the start
 - `[a-zA-Z0-9]+` is the main part which allow only small `a-z` capital `A-Z` and numbers with a minimum length of 1 to any extend.
 - `$` restrict the end

This can also done by using

    ^[\w\d]+$
Here

 - `\w` represent the alphabets
 - `\d` represent the digits

If we want to restrict the length to maximum 20 charactes,


    ^[a-zA-Z0-9]{1,20}$
 - The `{1,20}` indicate that the length can be between 1 and 20 including both.


