---
title: "Errors"
slug: "errors"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- **error** [*text*] [number *integer*]

## Parameters
| Parameter | Details |
| --------- | ------- |
| *unnamed* | A textual description of the error. |
| number    | The error's number, an integer (usually negative). |

## Throwing errors
You can throw your own errors using `error`. Execution stops at uncaught errors. By default, the error message is "An error has occurred." with error number -2700.

    error

> error "An error has occurred." number -2700 from «script» to item

You can pass a message with the error which overrides the default message.

    error "testing errors"

> error "testing errors" number -2700 from «script» to item

Error numbers can also be passed using the parameter `number`:

    error "testing errors" number -1

> error "testing errors" number -1 from «script» to item

If you include an error number but no error message, an error message is written for you.

    error number -1

> error "An error of type -1 has occurred." number -1 from «script» to item

Some error numbers are reserved for certain types of error.

    error number -42

> error "Too many files open" number -42 from «script» to item

## Catching errors
Error handling in AppleScript uses **`try` `on error`**. The code which may throw an error goes in the `try` block and any error handling code is in the `on error` block. The `on error` block is closed using `end try`.

`foo` is not defined, so throws an error. When an error occurs, the dialog is displayed.

    try
        foo
    on error
        display dialog "An error occurred"
    end try

It is possible to obtain the error message and error number using <code>on error *errormsg* number *errorno*</code> where *errormsg* and *errorno* are variable names for the error message and error number.

    try
        foo
    on error errormsg number errorno
        display dialog errormsg & errorno
    end try

> The variable foo is not defined.-2753

