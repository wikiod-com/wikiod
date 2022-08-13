---
title: "Parent selectors"
slug: "parent-selectors"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

As at the time of writing (Aug '16), parent selector (`&`) always refers to the full parent selector chain right till the top most level. It cannot be used to select just the immediate parent or the root most ancestor alone.

That is, in the below code `&#type1` would resolve to `#demo-container .content#type1` and not just `.content` or just `#demo-container`.

<!-- language: lang-css -->

     #demo-container {
      padding: 4px;
      border: 1px solid gray;
      #heading {
        padding: 4px;
        font-size: 20px;
      }
      .content {
        padding: 2px;
        font-size: 18px;
        &#type1 {
          color: chocolate;
        }
      }
    } 

## Basic parent selector
The `&` operator is the parent selector. When used in or as a selector, it is replaced with the full parent selectors (entire sequence of selectors right upto to the topmost level of a nested block) in the final CSS output.

It is useful when creating nested rules that require using the parent selector in a different way than default, like changing the order of the parent selector placement or to concatenate it with other selectors.


<!-- language: lang-css -->

    a {
      text-decoration: none;
      &:hover {
        text-decoration: underline;
      }
    }

Results in the following CSS where the parent selector `a` was concatenated with the `:hover` rule:

<!-- language: lang-css -->

    a {
      text-decoration: none;
    }
    a:hover {
      text-decoration: underline;
    }

One big advantage of using parent selectors wherever possible is the reduction of repetition of selectors.

## Changing the selector order within a nested block
Less allows the usage of the  parent selector (`&`) anywhere in a complex selector and thus allows changing styles when the current element is within another element which gives it a different context:

For example, in the below code the parent selector is placed at the end and thus it actually becomes the child's selector in the compiled CSS.

<!-- language: lang-css -->

    a {
      color: blue;
      .disabled-section & {
        color: grey;
      }
    }

**Compiled CSS:**

<!-- language: lang-css -->

    a {
      color: blue;
    }
    .disabled-section a {
      color: grey;
    }

## Select sibling elements that have the same class without repeating selector
Less doesn't put any restrictions on the number of times the parent selector (`&`) can be used in a complex selector and so, we can use it more than once like in the below examples to select sibling elements without the need to repeat the selector.

<!-- language: lang-css -->

    .demo {
      border: 1px solid black; /* add border to all elements with demo class */
      & + & { /* select all .demo that have another .demo sibling immediately prior */
        background: red;
      }
      & + & + & { /* select all .demo that have two .demo sibling immediately prior */
        background: chocolate;
      }
      & ~ & { /* select all .demo elements that have another .demo sibling prior */
        color: beige;
      }
    }

The above code when compiled will result in the following CSS:

<!-- language: lang-css -->

    .demo {
      border-left: 1px solid black;
    }
    .demo + .demo {
      background: red;
    }
    .demo + .demo + .demo {
      background: chocolate;
    }
    .demo ~ .demo {
      color: beige;
    }



