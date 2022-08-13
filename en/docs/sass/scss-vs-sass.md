---
title: "SCSS vs Sass"
slug: "scss-vs-sass"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

## Main Differences
Although people often say `Sass` as the name of this CSS-preprocessor, they often mean the `SCSS`-syntax. `Sass` uses the `.sass` file extension, while `SCSS`-`Sass` uses the `.scss` extension. They are both referred to as "Sass".

Speaking generally, the `SCSS`-syntax is more commonly used. `SCSS` looks like regular CSS with more capabilities, whereas `Sass` looks quite different to regular CSS. Both syntaxes have the same abilities.

# Syntax

The main differences are that `Sass` doesn't use curly brackets or semicolons, where `SCSS` does. `Sass` is also whitespace-sensitive, meaning you have to indent correctly. In `SCSS`, you can format and indent your rules as you please.

# SCSS:

<!-- language: lang-scss -->


    // nesting in SCSS
    .parent {
      margin-top: 1rem;
    
      .child {
        float: left;
        background: blue;
      }
    }

# SASS:
    
<!-- language: lang-sass -->

    // nesting in Sass
    .parent
      margin-top: 1rem
    
      .child
        float: left
        background: blue


After compilation, both will produce the same following CSS:


    .parent {
      margin-top: 1rem;
    }
    .parent .child {
      float: left;
      background: blue;
    }



## Maps
When it comes to maps, usually `SCSS` is the easier syntax. Because `Sass` is indent-based, your maps have to be saved in one line.

<!-- language-all: lang-scss -->

    // in Sass maps are "unreadable"
    $white: ( white-50: rgba(255, 255, 255, .1), white-100: rgba(255, 255, 255, .2), white-200: rgba(255, 255, 255, .3), white-300: rgba(255, 255, 255, .4), white-400: rgba(255, 255, 255, .5), white-500: rgba(255, 255, 255, .6), white-600: rgba(255, 255, 255, .7), white-700: rgba(255, 255, 255, .8), white-800: rgba(255, 255, 255, .9), white-900: rgba(255, 255, 255, 1 )


Because you can format your code on multiple lines with `SCSS`, you can format your maps to be more readable.

    // in SCSS maps are more readable
    $white: (
      white-50: rgba(255, 255, 255, .1),
      white-100: rgba(255, 255, 255, .2),
      white-200: rgba(255, 255, 255, .3),
      white-300: rgba(255, 255, 255, .4),
      white-400: rgba(255, 255, 255, .5),
      white-500: rgba(255, 255, 255, .6),
      white-600: rgba(255, 255, 255, .7),
      white-700: rgba(255, 255, 255, .8),
      white-800: rgba(255, 255, 255, .9),
      white-900: rgba(255, 255, 255, 1)
    );



## Comments
Comments in Sass vs. Scss are largely similar, except when multi-lines are concerned. `SASS` multi-lines are indentation-sensitive, while `SCSS` relies on comment terminators.

***

# Single-Line Comment

**style.scss**

```lang-scss
// Just this line will be commented!
h1 { color: red; }
```

**style.sass**
```lang-sass
// Exactly the same as the SCSS Syntax!
h1
  color: red
```

***

# Multi-Line Comment

**style.scss**

Initiator: `/*`

Terminator: `*/`

```lang-scss
/* This comment takes up
 * two lines.
 */
h1 {
   color: red;
}
```

This will style `h1` elements with the color **red**.

**style.sass**

Now, `SASS` has *two* initiators, but no respective terminators. Multiline comments in `SASS` are sensitive to **indentation levels**.

Initiators: `//` and `/*` 

```lang-sass
// This is starts a comment,
   and will persist until you 
   return to the original indentaton level.
h1
  color: red
```

This will style `h1` elements with the color **red**.

The same can be done with the `/*` Initiator:

```lang-sass
/* This is starts a comment,
   and will persist until you 
   return to the original indentaton level.
h1
  color: red
```

So there you have it! The main differences between comments in `SCSS` and `SASS`!

## Mixins
`Sass` tends to be the more "lazy" syntax. Nothing illustrates this nicer than how you define and include mixins.

# Defining a mixin

`=` is how you define a mixin in `Sass`, `@mixin` in `SCSS`.

<!-- language-all: lang-scss -->

    // SCSS
    @mixin size($x: 10rem, $y: 20rem) {
      width: $x;
      height: $y;
    }
      
    // Sass
    =size($x: 10rem, $y: 20rem)
      width: $x
      height: $y


# Including a mixin

`+` is how you include in `Sass`, `@include` in `SCSS`.

    // SCSS
    .element {
      @include size(20rem);
    }
    
    // Sass
    .element
      +size(20rem)



## Comparision between SCSS & SASS
 - `SCSS` syntax resembles more like a `CSS` syntax but `SASS` syntax is little bit different from `SCSS` but both produces exactly the same `CSS` code.
 - In `SASS` we are not ending the style properties with semicolon(`;`) but in SCSS we are ending the style properties with (`;`).
 - In `SCSS` we used paranthesis `{}` to close the style properties but in `SASS` we don't use `paranthesis`.
 - `Indentation` is very important in `SASS`. It will define the nested properties in the `class` or `id` of the element.
 - In `scss` we can define multiple variables in single line but in `SASS` we can't do.

[![enter image description here][1]][1]

  [1]: https://i.stack.imgur.com/jPxTn.png

## for loop syntax
With the release of sass 3.3 and plus version the @if and else conditions syntax got same. we can now use expressions with not only **scss** but also **sass**.

**sass syntax**

    @for $i from 1 through 3 {
      .item-#{$i} { width: 2em * $i; }
    }

Compiled to

    .item-1 {
      width: 2em;
    }
    .item-2 {
      width: 4em;
    }
    .item-3 {
      width: 6em;
    }
       

**scss syntax** 

    @for $i from 1 through 3 {
      .item-#{$i} { width: 2em * $i; }
    }

compiled to 

    .item-1 {
      width: 2em;
    }
    .item-2 {
      width: 4em;
    }
    .item-3 {
      width: 6em;
    }



