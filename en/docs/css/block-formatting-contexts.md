---
title: "Block Formatting Contexts"
slug: "block-formatting-contexts"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

> [A block formatting context is a part of a visual CSS rendering of a
> Web page. It is the region in which the layout of block boxes occurs
> and in which floats interact with each other.][1]


  [1]: https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Block_formatting_context MDN

## Using the overflow property with a value different to visible
<!-- language: lang-css -->

    img{
      float:left;
      width:100px;
      margin:0 10px;
    }
    .div1{
      background:#f1f1f1;
      /* does not create block formatting context */
    }
    .div2{
      background:#f1f1f1;
      overflow:hidden;
      /* creates block formatting context */
    }
[![enter image description here][1]][1]

https://jsfiddle.net/MadalinaTn/qkwwmu6m/2/


> [Using the overflow property with a value different to visible (its
> default) will create a new block formatting context. This is
> technically necessary — if a float intersected with the scrolling
> element it would forcibly rewrap the content.][2]


This example that show how a number of paragraphs will interact with a floated image is similar to [this example][2], on css-tricks.com.

  [2]: https://developer.mozilla.org/en-US/docs/Web/CSS/overflow MDN


  [1]: http://i.stack.imgur.com/ceEkU.png
  [2]: https://css-tricks.com/almanac/properties/o/overflow/

