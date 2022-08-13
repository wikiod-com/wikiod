---
title: "Check for NULL or Blank"
slug: "check-for-null-or-blank"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Check for NULL or Blank fields
This line of code demonstrate how to check if a specific field is NULL or has blank value


    =IIF(IsNothing(Fields!UserEmail.Value) OR Fields!UserEmail.Value = "",
     "Empty", "Not Empty")

This line of code checks if the field is `NULL`

    IsNothing(Fields!UserEmail.Value)

This line of code checks if the field contains blank value ""

 

    Fields!UserEmail.Value = ""

## Check for NULL or Empty fields - shortcut
To get a shorter version of the Null or Empty check, use an "= Nothing" comparison.

    Iif(Fields!UserEmail.Value = Nothing, "Null or Empty", "Not Null or Empty")

The "= Nothing" will check simultaneously against Null or Empty, giving a more compact expression.  This works for String, Numeric, and Boolean.  From MSDN:

    Nothing represents the default value of a data type. The default value depends on whether the variable is of a value type or of a reference type.
    A variable of a value type directly contains its value. Value types include all numeric data types, Boolean, Char, Date, all structures, and all enumerations. A variable of a reference type stores a reference to an instance of the object in memory. Reference types include classes, arrays, delegates, and strings. For more information, see Value Types and Reference Types.
    If a variable is of a value type, the behavior of Nothing depends on whether the variable is of a nullable data type. To represent a nullable value type, add a ? modifier to the type name. Assigning Nothing to a nullable variable sets the value to null.

String will equate to Nothing if they are null or the empty string "".

Numerics will equate to Nothing if they are 0.

Booleans will equate to Nothing if they are False.

For a full list of the types and default values, check the (current) MSDN pages for [Nothing][1].


  [1]: https://msdn.microsoft.com/en-us/library/0x9tb07z.aspx

