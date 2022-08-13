---
title: "Nesting"
slug: "nesting"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Basic nesting


Whenever you declare a new rule *inside* another rule it is called nesting. With basic nesting, as shown below, the nested selector will be compiled as a new CSS selector with all its parents prepended, separated by a space.

```scss

// SCSS
.parent {
  margin: 1rem;

  .child {
    float: left;
  }
}

// CSS output
.parent {
  margin: 1rem;
}

.parent .child {
  float: left;
}
```

## Nesting depth
Nesting is a very powerful feature, but should be used with caution. It can happen quite easily and quickly, that you start nesting and carry on including all children in a nest, of a nest, of a nest. Let me demonstrate:

```
// SCSS
header {
  // [css-rules]

  .holder {
    // [css-rules]

    .dropdown-list {
      // [css-rules]

      ul {
        // [css-rules]

        li {
          margin: 1rem 0 0 1rem;
        }
      }
    }
  }
}

// CSS output of the last rule
header .holder .dropdown-list ul li {
  margin: 1rem 0 0 1rem;
}
```

# Problems

### Specificity

The `li` from the example above has a `margin` set. Let's say we want to override this in a media-query later on.

```
@media (max-width: 480) {

  // will not work
  .dropdown-list ul li {
    margin: 0;
  }

  // will work
  header .holder .dropdown-list ul li {
    margin: 0;
  }
}
```

So by nesting too deep consequently you'll have to nest deep again whenever you want to overwrite a certain value. Even worse, this is often where the rule `!important` comes to use.

```
@media (max-width: 480) {

  // BIG NO-NO, don't do this
  .dropdown-list ul li {
    margin: 0 !important;
  }
```

*Why is the `!important`-rule is a bad idea*

You should write your SCSS in a good fashion that these workarounds aren't even necessary in the first place. Using `!important` on such a minor issue already will lead you down a rabbit hole!

### Reusability

This is fairly similar to the specificity problem, but worth pointing out separately. If you style something like a button or even a dropdown, you might want to reuse those styles somewhere else on your page.

By nesting too deeply your styles are only bound to the elements sitting inside the most outer parent (the element at the top of your SCSS). This leads you to copy styles and paste them somewhere else again. Possibly in an other nested rule.

Your stylesheets will become larger and larger and more difficult to maintain.

# How deep should you nest?

Most styleguides set the maximum depth to 2. This is good advice in general, as there are only very few occasions where you'd want to nest deeper. Most of the time, 2 is enough.

## Nesting with @at-root
Nesting is probably most often used to create more specific selectors, but it can also be used simply for code organization. Using the `@at-root` directive, you can ‘jump out’ of where you nest it in your Sass, bringing you back at the top level. Doing this allows you to keep styles grouped without creating more specificity than you need.

For example, you could to something like this :

    .item {
        color: #333;

        @at-root {
            .item-wrapper {
                color: #666;

                img {
                    width: 100%;
                }
            }
        }
        
        .item-child {
            background-color: #555;
        }
    }

That would compile to this :

    .item {
      color: #333;
    }
    .item-wrapper {
      color: #666;
    }
    .item-wrapper img {
      width: 100%;
    }
    .item .item-child {
      background-color: #555;
    }

By doing this, all of our styles related to the `.item` class are together in the SCSS, but we don't necessarily need that class in every selector.

**Excluding contexts**

By default declarations inside `@at-root` will appear in any context. This means that rules inside a `@media` block for instance will remain there.

    @media print {
      .item-wrapper {
        @at-root {
          .item {
            background: white;
          }
        }
      }
    }
    
    // Will compile to
    @media print {
      .item {
        background: white;
      }
    }

This is not always desired behavior, so you can exclude the media context, by passing `media` to the the `without` option of the `@at-root` directive.

    @at-root (without: media) {..

For more information, see the [**official documentation**][1]

  [1]: http://sass-lang.com/documentation/file.SASS_REFERENCE.html#at-root

## The parent selector (&)
Nesting is great for keeping related selectors together to make it easier for future developers to understand your code. The parent selector, represented by an ampersand ("&") can help do that in more complex situations. There are several ways its can be used.

Create a new selector that requires both the parent selector and another on the same element by placing the new selector directly after a parent selector.

```scss

// SCSS
.parent {

  &.skin {
    background: pink;
  }
}
```
```css
// CSS output
.parent.skin {
  background: pink;
}
```

Have the parent appear after a nested selector in the compiled CSS by placing the parent selector _after_ the nested selector.

```scss

// SCSS
.parent {

  .wrapper & {
    border: 1px solid black;
  }
}
```
```css
// CSS output
.wrapper .parent {
  border: 1px solid black;
}
```

## States and pseudo-elements

Besides using nesting for classes and children, nesting with the parent selector is also commonly used to combine the states of `:active`, `:hover` and `:focus` for links.

```
// SCSS
a {
  color: blue;

  &:active,
  &:focus {
    color: red;
  }

  &:visited {
    color: purple;
  }
}
```
```css
// CSS output
a {
  color: blue;
}

a:active,
a:focus {
  color: red;
}

a:visited {
  color: purple;
}
```

Similarly, you can style pseudo-elements by nesting with the parent selector.

```scss
// SCSS
.parent {

  &::after {
    display: table;
    clear: both;
    content: '';
  }

  &::only-child {
    font-weight: bold;
  }
}
```
```css
// CSS output
.parent::after {
  display: table;
  clear: both;
  content: '';
}

.parent::only-child {
  font-weight: bold;
}
```

## Nesting properties
Some CSS properties belong to a namespace, for instance `border-right` belongs to the `border` namespace. To write less, we can utilize property nesting, and skip these prefixes, even on multiple levels.

If we needed to create a border on the right and left of a class named `.borders` we could write this:

```css
 //SCSS
 .borders {
    border: 2px dashed blue;
    border: {
        left: 1px solid black;
        right: 1px solid red;
    }
  }

 // CSS output 
 .borders {
    border: 2px dashed blue;  
    border-left: 1px solid black;
    border-right: 1px solid red;
  }
 
```
This saves us having to write `border-right` and `border-left`, however we are still writing repetitive code with the lines `1px solid black` and `1px solid red`. We can write still less repetitive CSS with the folowing: 

    // SCSS
    .borders {
      border: 2px dashed blue {
        left: 1px solid black;
        right: {
          color: red;
        }
      }
    }

    // CSS output
    .borders {
       border: 2px dashed blue;
       border-left: 1px solid black;
       border-right-color: red;
    }

