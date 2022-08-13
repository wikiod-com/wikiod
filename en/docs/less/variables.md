---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Introduction
In Less, unlike Sass or Shell, the variables are declared by having names starting with a `@` symbol. For example:

<!-- language: lang-css -->

    @sky-blue: #87ceeb;
    
    body {
      background-color: @sky-blue;
    }

The above example gives you:

<!-- language: lang-css -->

    body {
      background-color: #87ceeb;
    }

Here it explains how to declare a variable and make use of them.

## Operations in Colour
Consider the following example:

<!-- language: lang-css -->

    @sky-blue: #87ceeb;
    @dark-sky-blue: @sky-blue + #333;
    
    body {
      background-color: @dark-sky-blue;
    }

The above example gives you:

<!-- language: lang-css -->

    body {
      background-color: #baffff;
    }

Here it explains how to declare a variable and also make operations on a particular variable as well.

## Concatenate value of two or more variables
To concatenate the value of two or more variables into a single string and print it as the output, we need to make use of interpolation.

The following Less code,

<!-- language: lang-css -->

    #demo:after {
      @var1: Hello;
      @var2: World!!!;
      content: "@{var1} @{var2}";
    }

when compiled would set **"Hello World!!!"** as value to the `content` property. Below is the compiled CSS:

<!-- language: lang-css -->

    #demo:after {
      content: "Hello World!!!";
    }

---

If the value of two or more variables just need to be placed next to each other in space separated manner then interpolation is not required.

<!-- language: lang-css -->

    #demo {
      @top: 4px;
      @right: 2px;
      @bottom: 6px;
      @left: 4px;
      padding: @top @right @bottom @left;
    }

When the above code is compiled, it would produce the following CSS.

<!-- language: lang-css -->

    #demo {
      padding: 4px 2px 6px 4px;
    }

This approach will not work when there should be no space between the variable values (or) when the resultant string needs to be within quotes. For those cases, usage of interpolation would be mandatory.

## Referencing a Variable Within a CSS Function
By default, LESS will use its own `calc()` unless told otherwise. So:
    
    @column-count: 2;
    
    .class-example {
        width: calc(100% / @column-count);
    }

Would compile to this:

    .class-example {
        width: 50%;
    }

While it is our desired width, LESS has used it's own `calc()` function to calculate the `width`. The `calc()` function never makes it to our CSS. If you would like LESS to not use its `calc()` function, you need to escape your values like this:

    width: calc(~"100% - @{column-count}");

Here we've prepended our values with a `~` and wrapped them in quotation marks. Variables can be referenced as well, but you must wrap the variable name in `{ }` brackets. This allows you use the CSS `calc()` function for more complex calculations like this:
    
    @column-count: 2;
    @column-margin: 24px;
    
    .class-example {
        width: calc("~(100% / @{column-count}) - @{column-margin}");
    }

This compiles to:

    .class-example {
        width: calc((100/2) - 24px);
    }


## Variables can make your recursive work easy
**Variable declaration**

    @buttonColor: #FF0000;


/* Now you can use @buttonColor variable with css Functions. */

    .product.into.detailed {
        additional-attributes{
            .lib-table-button(
             background-color: @buttonColor;
             );
        }
    }

Here function `lib-tabgle-button` used variable to set background color

