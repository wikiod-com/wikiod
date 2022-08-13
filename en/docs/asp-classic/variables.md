---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Declaring
Creating variables in VBScript can be done by using the Dim, Public, or Private statement. It is best practice to put at the top of the script "Option Explicit" which forces you to explicitly define a variable.

You can declare one variable like this:

<!-- language-all: lang-vb -->

    Option Explicit
    Dim firstName

Or you can several variables like this:

    Option Explicit
    Dim firstName, middleName, lastName

If you do not have the option explicit statement, you can create variables like so:

    firstName="Foo"
This is **NOT** recommended as strange results can occur during the run time phase of your script. This happens if a typo is made later when reusing the variable.

To create an array, simply declare it with how many elements in the parameter:

    Option Explicit
    Dim nameList(2)
This creates an array with three elements

To set array elements, simply use the variable with the index as parameter like so:

    nameList(0) = "John"

VBScript also supports multi-dimensional arrays:

    Option Explicit
    Dim gridFactors(2, 4)

## Variable Types
VBScript is a weakly typed language; variables are all of type [variant](https://msdn.microsoft.com/en-us/library/9e7a57cf(v=vs.84).aspx), though they usually have an implied [subtype](https://msdn.microsoft.com/en-us/library/9e7a57cf(v=vs.84).aspx) denoting the data they hold.

This means that your variable, no matter what you call it, can hold any value:

    Dim foo
    foo = "Hello, World!"
    foo = 123.45
    foo = #01-Jan-2016 01:00:00#
    foo = True

Note that the above is perfectly valid code, though mixing your variables like this is amazingly poor practice.

The string subtype is always assigned by using speech marks `" "`. Unlike JavaScript and other languages, the apostrophe does not provide the same functionality.

Numbers in VBScript can include any format of number, but do have a particular subtype based on their value and whether they contain a decimal point or not.

Dates use the `# #` specifiers. Be aware that formats for a numeric date style  (e.g. 01/01/2016) retains an American date format, so `#05/06/2016#` is 6th May, not 5th June. This can be circumnavigated by using a `#dd-mmm-yyyy#` format, as in the example above.

Boolean variables contain `True` or `False` values.

As explained earlier, arrays are dimensioned using a set of parentheses to define the number of elements and ranks (rows and columns), for instance:

    Dim myArray(3, 4)

All elements in arrays are of type variant, allowing every single element to be of any subtype.  This is very important when you need to perform tasks such as reading data from a record set or other object. In these cases, data can be directly assigned to a variable, for instance, when being returned from a record set...

    Dim myData
    ....
    myData = rsMyRecordset.GetRows()
    ....
    Response.Write(myData(3,2))

One final type that requires some explanation is the `Object` type. Objects are basically pointers to the memory location of the object itself. Object types must be `Set`...

    Dim myObj
    Set myObj = Server.CreateObject("ADODB.ecordSet")

