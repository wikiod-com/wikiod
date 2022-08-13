---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Writing a for-each loop
A for-each loop in Less has the same key components as a for loop except for the following differences:

 - A variable which contains the list of items that has to be iterated over.
 - An `extract()` function to extract each item in the variable based on the loop's index.
 - A `length()` function to calculate the length of the array (that is, the no of items in the  list) and use it in the primary mixin call (for `[initialization]`).

Below is a sample for-each loop written in Less that iterates through each item in the `@images` variable, creates a `#id` selector where the `id` is the same as the item/image name and also sets the background image property for it.

<!-- language: lang-css -->

    @images: cat, dog, apple, orange; /* the array list of items */

    .for-each-loop(@index) when (@index > 0) { /* recursive mixin call with guard - condition */
      
      @image: extract(@images, @index); /* extract function to fetch each item from the list */
      
     /* the statement */
      #@{image} {
        background-image: url("http://mysite.com/@{image}.png");
      }
      /* end of the statement */

      .for-each-loop(@index - 1); /* the next iteration's call - final-expression */
    }

    .for-loop(length(@images)); /* the primary call with length() function - initialization */

**Compiled CSS:**

<!-- language: lang-css -->

    #orange {
      background-image: url("http://mysite.com/orange.png");
    }
    #apple {
      background-image: url("http://mysite.com/apple.png");
    }
    #dog {
      background-image: url("http://mysite.com/dog.png");
    }
    #cat {
      background-image: url("http://mysite.com/cat.png");
    }

## Writing a simple for loop
Usage of loops is an excellent way to keep the code DRY and avoid repetition. Unlike in Sass, there are no built-in `@for` or `@each` directive in Less for writing loops but it can still be written using recursive mixins. A recursive mixin is nothing but a mixin which keeps calling itself.

There are four key components to a loop written using Less and they are as follows:

 - A mixin with guard expressions. The guard is used to terminate the loop when the loop's exit criteria is met. In terms of a JavaScript for loop (`for([initialization]; [condition]; [final-expression])`), the guard is the `[condition]`.
 - A primary call to the mixin to execute the first iteration. This primary call to the mixin can be made from within a selector block (if the mixin doesn't have a selector wrapping all its contents) or from outside a selector block (if the mixin has a selector wrapping its contents). In terms of a JavaScript for loop, this primary call serves as the `[initialization]` as it sets the base value for the counter-like variable.
 - A call to the mixin from within itself to make it recursive. This call typically passes an incremented or a decremented value of the counter variable as the argument. Thus it invokes the subsequent iterations. In terms of a JS for loop, this does the `[final-expression]` along with the next call.
 - Last but not the least, the other contents of the mixin which is equivalent to the `statement` in a typical for loop syntax.

Below is a simple for loop written in Less that creates multiple `#img*` selectors (where * is a number) and also sets the `background-image` property as `image*.png`.

<!-- language: lang-css -->

    .for-loop(@index) when (@index > 0) { /* recursive mixin with guard expression - condition */

      /* the statement */
      #img@{index} {
        background-image: url("http://mysite.com/image@{index}.png");
      }
      /* end of the statement */

      .for-loop(@index - 1); /* the next iteration's call - final-expression*/
    }
    .for-loop(3); /* the primary call - initialization */

**Compiled CSS:**

<!-- language: lang-css -->

    #img3 {
      background-image: url("http://mysite.com/image3.png");
    }
    #img2 {
      background-image: url("http://mysite.com/image2.png");
    }
    #img1 {
      background-image: url("http://mysite.com/image1.png");
    }

