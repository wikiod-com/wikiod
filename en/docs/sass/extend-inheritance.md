---
title: "Extend  Inheritance"
slug: "extend--inheritance"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 - `@extend .<className>`
 - `@extend .<className>, .<className>`
 - `@extend .<className> !optional`
 - `@extend .<className>, .<className> !optional`

## Parameters
| Parameter | Details |
| ------ | ------ |
| className | The class that you want to extend. |

Sass' `@extend` rule allows you to share CSS properties across multiple classes, keeping code DRY and easier to read.

## Extend a Class
    .message
      color: white

    .message-important
      @extend .message
      background-color: red

This will take all of the styles from `.message` and add them to `.message-important`. It generates the following CSS:

    .message, .message-important {
      color: white;
    }

    .message-important {
      background-color: red;
    }

## Extend from Multiple Classes
    .message
      color: white

    .important
      background-color: red

    .message-important
      @extend .message, .important

In the above code `@extend` is used in one line to add multiple classes' code to `.message-important`, however, it is possible to use one extend per line like this:

    .message-important
        @extend .message
        @extend .important

Either one of these methods will generate the following CSS:

    .message, .message-important {
      color: white;
    }

    .important, .message-important {
      background-color: red;
    }

## Chaining Extends
    .message
      color: white
      background: black

    .message-important
      @extend .message
      font-weight: bold
    
    .message-error
      @extend .message-important
      font-style: italic

This code causes `.message-error` to extend from `.message-important`, which means that it will contain code from both `.message-important` and `.message`, since `.method-important` extends from `.message`. This results in the following CSS:

    .message, .message-important, .message-error {
      color: white;
      background: black;
    }

    .message-important, .message-error {
      font-weight: bold;
    }

    .message-error {
      font-style: italic;
    }

**Disclaimer: Make sure that the class(es) you are extending from occur only *once* in the code, otherwise Sass may generate some messy, convoluted CSS.**

## Optional Extends
Sometimes, you may want an `@extend` to be optional, and not require the specified class to exist in your code.

    .message-important
      @extend .message !optional
      background: red

This will result in the following CSS:

    .message-important {
      background: red;
    }

**Disclaimer: This is useful during development when you may not have all of your code written yet and don't want errors, but it should probably be removed in production because it could lead to unexpected results.**

## Placeholders
Sometimes you will create classes that won't be used in their own right, rather only be extended inside other rule sets.  This means that the compiled CSS file will be larger than it needs to be.  Placeholder selectors solve this problem. 

Placeholder selectors are similar to class selectors, but they use the percent character (%) instead of the (.) used for classes.  They will not show up in the compiled CSS.

    %button {
        border: 5px solid black;
        border-radius: 5px;
        margin: 0;
    } 

    .error-button {
        @extend %button;
        background-color: #FF0000;
    }
    
    .success-button {
        @extend %button;
        background-color: #00FF00;
    }

This will compile to the following CSS:
    
    .error-button, .success-button {
        border: 5px solid black;
        border-radius: 5px;
        margin: 0;
    }

    .error-button {
        background-color: #FF0000;
    }
    
    .success-button {
        background-color: #00FF00;
    }

## Extending the parent
Typically trying to extend the parent like so:

    .parent {
      style: value;

      @extend &;
    }
Will result in an error, stating that the parent cannot be extended. This makes sense, but there's a workaround. Simply store the parent selector in a variable.

    .parent {
      $parent: &;
      style: value;
      @extend #{&};
    }

There's no benefit to doing this in the above example, however this gives you the power to wrap parent styles from within an included mixin.

