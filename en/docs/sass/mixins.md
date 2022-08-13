---
title: "Mixins"
slug: "mixins"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- `@mixin mixin-name ($argument1, $argument, ...){ ... }`

## Mixin with variable argument
There are some cases in mixins where there can be single or multiple arguments while using it. Let's take a case of `border-radius` where there can be single argument like `border-radius:4px;` or multiple arguments like `border-radius:4px 3px 2px 1px;`.

Traditional with Keyword Arguments mixing will be like below:-


    @mixin radius($rad1, $rad2, $rad3, $rad4){
     -webkit-border-radius: $rad1 $rad2 $rad3 $rad4;
     -moz-border-radius: $rad1 $rad2 $rad3 $rad4;
     -ms-border-radius: $rad1 $rad2 $rad3 $rad4;
     -o-border-radius: $rad1 $rad2 $rad3 $rad4;
     border-radius: $rad1 $rad2 $rad3 $rad4;
    }

And used as

    .foo{
        @include radius(2px, 3px, 5px, 6px)
    }

The above example is complex (to code, read and maintain) and if you can't pass only one value or two values, it will throw an error, and to use **one, two or there** values you have to define three other mixins.

By using  **variable Argument** you don't have to worry about how many arguments can you pass. Variable arguments can be declared by defining a variable name followed by **three dots(...)**. Following is an example of a variable argument.

    @mixin radius($radius...)
    {  
        -webkit-border-radius: $radius;
        -moz-border-radius: $radius;
        -ms-border-radius: $radius;
        -o-border-radius: $radius;
        border-radius: $radius;
    }

And used as

    .foo{
        @include radius(2px 3px 5px 6px)
    }
    .foo2{
        @include radius(2px 3px)
    }
    .foo3{
        @include radius(2px)
    }

The above example is much simpler (to code, maintain and read), you need not worry about how many arguments are about to come - is it **one or more than one**.

If there is more than one argument and in any case you want to access the second argument, you can do it by  writing *`propertyname : nth(variable_name, 2)`*. 




## Create and use a mixin
To create a mixin use the `@mixin` directive. 
    
    @mixin default-box ($color, $borderColor) {
        color: $color;
        border: 1px solid $borderColor;
        clear: both;
        display: block;
        margin: 5px 0;
        padding: 5px 10px;
    }

You can specify a list of arguments inside a parenthesis following the mixin's name. Remember to start your variables with `$` and separate them with commas. 

To use the mixin in another selector, use the `@include` directive.

    footer, header{ @include default-box (#ddd, #ccc); }
    

The styles from the mixin will now be used in the `footer` and `header`, with the value `#ccc` for the `$color` variable and `#ddd` for the `$borderColor` variable.

## Optional arguments
SASS's optional arguments let you use a parameter only if you specify its value; otherwise, it will be ignored.
Let's take an example of the following mixin:


    @mixin galerie-thumbnail ($img-height:14em, $img-width: null) {
        width: $img-width;
        height: $img-height;
        outline: 1px solid lightgray;
        outline-offset: 5px;
    }

So a call to 

    .default { 
      @include galerie-thumbnail; 
    }
    .with-width { 
      @include galerie-thumbnail($img-width: 12em);
    }
    .without-height { 
      @include galerie-thumbnail($img-height: null);
    }

will simply output the following in the CSS file:

    .default {
      height: 14em;
      outline: 1px solid lightgray;
      outline-offset: 5px;
    }
    
    .with-width {
      width: 12em;
      height: 14em;
      outline: 1px solid lightgray;
      outline-offset: 5px;
    }
    
    .without-height {
      outline: 1px solid lightgray;
      outline-offset: 5px;
    }
    
SASS doesn't output properties with `null` as their value, which is very helpful when we need to include an optional argument in our call or not.

## @content directive
Mixins can be passed a block of SASS compliant code, which then becomes available within the mixin as the `@content` directive.

    @mixin small-screen {
      @media screen and (min-width: 800px;) {
        @content;
      }
    }

    @include small-screen {
      .container {
        width: 600px;
      }
    }

And this would output:

      @media screen and (min-width: 800px;) {
        .container {
          width: 600px;
        }
      }

Mixins can use the `@content` directive and still accept parameters.

    @mixin small-screen($offset) {...

## Sensible defaults
SASS gives you the ability to omit any parameter except the ones you want to overwrite of course. Let's take again the `default-box` example:

    @mixin default-box ($color: red, $borderColor: blue) {
        color: $color;
        border: 1px solid $borderColor;
        clear: both;
        display: block;
        margin: 5px 0;
        padding: 5px 10px;
    }
Here we'll now call the mixin having overwritten the second parameter 

    footer, header{ @include default-box ($borderColor: #ccc); }
the value of `$borderColor` is `#ccc`, while `$color` stays `red`


