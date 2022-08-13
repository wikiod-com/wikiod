---
title: "Guards (for writing conditional mixins)"
slug: "guards-for-writing-conditional-mixins"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
 - **Mixin with Guards**
 - .mixin-name(&lt;arguments>) when &lt;is-negation> (&lt;ref-variable> &lt;operator> &lt;value>)
 - **CSS Guards**
 - &lt;selector> when &lt;is-negation> (&lt;ref-variable> &lt;operator> &lt;value>)

## Parameters
| Parameter | Details |
| ------ | ------ |
| arguments | The variables that are passed to the parametric mixin.  Arguments are optional. |
| is-negation | This indicates whether the guard condition is a `not` condition or not. For example `when not (@type = error)` means the mixin will be used whenever the value of `@type` is not *error*. |
| ref-variable | This is the variable whose value determines which mixin's properties should be applied to the element. This is mandatory.  |
| operator | This is the operator that is used for evaluating the condition. It can be `=`, `>`, `<`, `>=`, `<=`. This is not mandatory. When the operator and value is not given, the compiler assumes the condition to be `&lt;ref-variable> = true`.  |
| value | The value that is used for evaluating the condition. This is optional but becomes mandatory when an operator is provided. |

## Style an element based on a variable's value
<!-- language: lang-css -->

    .set-colors(@type) when (@type = error) {
      @base-color: #d9534f;
      background: @base-color;
      color: contrast(@base-color, lighten(@base-color, 25%), darken(@base-color, 25%));
      border: 1px solid contrast(@base-color, lighten(@base-color, 25%), darken(@base-color, 25%));
    }
    .set-colors(@type) when (@type = info) {
      @base-color: #5bc0de;
      background: @base-color;
      color: contrast(@base-color, lighten(@base-color, 5%), darken(@base-color, 5%));
      border: 1px solid contrast(@base-color, lighten(@base-color, 5%), darken(@base-color, 5%));
    }
    .set-colors() {
      background: white;
      color: black;
      border: 1px solid black;
    }
    
    .error-message {
      .set-colors(error);
    }
    .info-message {
      .set-colors(info);
    }
    .default-div {
      .set-colors;
    }

In the above example, the `background`, `border` and `color` are assigned based on the type of element. If the element is a default text `div` then the background will be white whereas the text and border would be black. If it is an "error" message display `div` or an "info" message display `div` then the colors are assigned based on the type.

The compiled CSS output would be as follows:

<!-- language: lang-css -->

    .error-message {
      background: #d9534f;
      color: #f0b9b8;
      border: 1px solid #f0b9b8;
    }
    .info-message {
      background: #5bc0de;
      color: #46b8da;
      border: 1px solid #46b8da;
    }
    .default-div {
      background: white;
      color: black;
      border: 1px solid black;
    }

