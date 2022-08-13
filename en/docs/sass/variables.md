---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Syntax
 - $*variable_name*: *value*;

## SCSS
Just as in Sass, SCSS variables are used to store a value which will be used multiple times throughout a SCSS document.

Variables are mostly used to store frequently-used property values (such as fonts and colors), but can be used for any value of any property.

SCSS uses the `$` symbol to declare a variable.

    $font-stack: Helvetica, sans-serif;
    $primary-color: #000000;
    
    body {
      font-family: $font-stack;
      color: $primary-color;
    }

You can use `!default` when declaring a variable if you want to assign a new value to this variable only if it hasn't been assigned yet:

    $primary-color: blue;
    $primary-color: red !default; // $primary-color is still "blue"
    $primary-color: green;        // And now it's green.
    

## Sass
Variables are used to store a value once which will be used multiple times throughout a Sass document.

They are mostly used for controlling things such as fonts and colors but can be used for any value of any property.

Sass uses the `$` symbol to make something a variable.

    $font-stack: Helvetica, sans-serif
    $primary-color: #000000
    
    body
      font-family: $font-stack
      color: $primary-color

## Variable Scope
Variables exist within a specific scope, much like in in JavaScript.

If you declare a variable outside of a block, it can be used throughout the sheet.

    $blue: dodgerblue;

    .main {
        background: $blue;

        p {
            background: #ffffff;
            color: $blue;
        }
    }

    .header {
        color: $blue;
    }

If you declare a variable within a block, it can only be used in that block.

    .main {
        $blue: dodgerblue;

        background: $blue;

        p {
            background: #ffffff;
            color: $blue;
        }
    }

    .header {
        color: $blue; // throws a variable not defined error in SASS compiler
    }

Variables declared at the sheet level (outside of a block) can also be used in other sheets if they are [imported][1].

[1]: https://www.wikiod.com/sass/partials-and-import#Importing

## Localize Variables with @at-root directive
[@at-root][1] directive can be used to localize variables.

```
$color: blue;

@at-root {
  $color: red;

  .a {
    color: $color;
  }
  .b {
    color: $color;
  }
}

.c {
  color: $color;
}
```

is compiled to:

```
.a {
  color: red;
}

.b {
  color: red;
}

.c {
  color: blue;
}
```

  [1]: http://sass-lang.com/documentation/file.SASS_REFERENCE.html#at-root

## Interpolation
Variables can be used in string interpolation. This allows you to dynamically generate selectors, properties and values. And the syntax for doing so a variable is `#{$variable}`.

    $className: widget;
    $content: 'a widget';
    $prop: content;
    
    .#{$className}-class {
      #{content}: 'This is #{$content}';
    }
    // Compiles to

    .widget-class {
      content: "This is a widget";
    }

You cannot, however use it to dynamically generate names of mixins or functions.

## Variables in SCSS
In SCSS variables begin with `$` sign, and are set like CSS properties.

`$label-color: #eee;`

They are only available within nested selectors where they’re defined. 

    #menu {
        $basic-color: #eee;
        color: $basic-color;
    }

If they’re defined outside of any nested selectors, then they can be used everywhere. 

    $width: 5em;

    #menu {
      width: $width;
    }

    #sidebar {
      width: $width;
    }

They can also be defined with the `!global` flag, in which case they’re also available everywhere.

    #menu {
      $width: 5em !global;
      width: $width;
    }

    #sidebar {
      width: $width;
    }

It is important to note that variable names can use hyphens and underscores interchangeably. For example, if you define a variable called `$label-width`, you can access it as `$label_width`, and vice versa.

