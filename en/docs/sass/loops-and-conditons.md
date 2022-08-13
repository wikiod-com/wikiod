---
title: "Loops and Conditons"
slug: "loops-and-conditons"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## While loop
The `@while` directive will loop over a block of code until the condition specified becomes false. In the following example, this loop will run until `$font-size <= 18` while incrementing the value for `$font-size` by 2.

    $font-size: 12;
    
    @while $font-size <= 18 {
        .font-size-#{$font-size} {
            font-size: ($font-size * 1px);
        }
    
        $font-size: $font-size + 2;
    }

Output of above code   

    .font-size-12 {
      font-size: 12px;
    }
    
    .font-size-14 {
      font-size: 14px;
    }
    
    .font-size-16 {
      font-size: 16px;
    }
    
    .font-size-18 {
      font-size: 18px;
    }

## for loop
The `@for` directive allows you to loop through some code for a set amount of iterations and has two forms:

- `@for <var> from <start> through <end> {}`
- `@for <var> from <start> to <end> {}`

The difference in the two forms is the *through* and the *to*; the *through* keyword will include the `<end>` in the loop where *to* will not; using *through* is the equivalent of using `>=` or `<=` in other languages, such as C++, JavaScript, or PHP.

**Notes**

- Both `<start>` and `<end>` must be integers or functions that return integers.
- When `<start>` is greater than `<end>` the counter will decrement instead of increment.

**SCSS Example**

    @for $i from 1 through 3 {
      .foo-#{$i} { width: 10px * $i; }
    }
    
    // CSS output
    .foo-1 { width: 10px; }
    .foo-2 { width: 20px; }
    .foo-3 { width: 30px; }


## Conditional directive (if)
The `@if` control directive evaluates a given expression and if it returns anything other than `false`, it processes its block of styles.

**Sass Example**

    $test-variable: true !default
    
    =test-mixin
      @if $test-variable
        display: block
      @else
        display: none
    
    .test-selector
      +test-mixin

**SCSS Example**

    $test-variable: true !default
    
    @mixin test-mixin() {
      @if $test-variable {
        display: block;
      } @else {
        display: none;
      }
    }
    
    .test-selector {
      @include test-mixin();
    }

The above examples produces the following CSS:

    .test-selector {
      display: block;
    }

## Each loop
The @each directive allows you to iterate through any list or map. It takes the form of `@each $var or <list or map> {}` where `$var` can be any variable name and `<list or map>` can be anything that returns a list or map.

In the following example, the loop will iterate through the `$authors` list assigning each item to `$author`, process its block of styles using that value of `$author`, and proceed to the next item in the list.

**SCSS Example**

    $authors: "adam", "steve", "john";
    @each $author in $authors {
        .photo-#{$author} {
          background: image-url("avatars/#{$author}.png") no-repeat
        }
    }

**CSS Output**

    .photo-adam {
      background: image-url("avatars/adam.png") no-repeat;
    }
    .photo-steve {
      background: image-url("avatars/steve.png") no-repeat;
    }
    .photo-john {
      background: image-url("avatars/john.png") no-repeat;
    }

## Multiple Assignment

Multiple assignment allows you to gain easy access to all of the variables by declaring multiple variables in the `@each` directive.

**Nested Lists**

To have easy access to all the nested elements, you may declare separate variables to match each nested element. Be sure you have the correct amount of variables and nested elements. In the following example, an each loop is iterating through a list of three elements each of which contains three elements nested. Having the wrong amount of declared variables will result in a compiler error.

    @each $animal, $color, $cursor in (puma, black, default),
                                      (sea-slug, blue, pointer),
                                      (egret, white, move) {
      .#{$animal}-icon {
        background-image: url('/images/#{$animal}.png');
        border: 2px solid $color;
        cursor: $cursor;
      }
    }

**Maps**

Multiple assignment works for Maps as well but is limited to only two variables, a variable to access the key and a variable to access the value. The names `$key` and `$value` are arbitary in the following example:

    @each $key, $value in ('first': 1, 'second': 2, 'third': 3) {
      .order-#{$key} {
        order: $value;
      }
    }

## Each Loop with maps/ list values
In the below example value in map `$color-array` is treated as list of pairs.

SCSS Input

<!-- language: lang-scss -->

    $color-array:(
      black: #4e4e4e,                       
      blue: #0099cc,
      green: #2ebc78
    );
    @each $color-name, $color-value in $color-array {
      .bg-#{$color-name} {
        background: $color-value;
      }
    }

CSS Output

<!-- language: lang-css -->
    
    .bg-black {
      background: #4e4e4e;
    }
    
    .bg-blue {
      background: #0099cc;
    }
    
    .bg-green {
      background: #2ebc78;
    }

