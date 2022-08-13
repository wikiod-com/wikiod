---
title: "CSS Image Sprites"
slug: "css-image-sprites"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Syntax
 - **//Using background-position**  
   background: url("sprite-image.png");  
   background-position: -20px 50px;    
 - **//Background property shorthand**  
   background: url("sprite-image.png") -20px 50px;

For some use cases, sprites are slowly falling out of favor, being replaced by icon webfonts or [SVG images](https://www.wikiod.com/svg/getting-started-with-svg).

## A Basic Implementation
**What's an image sprite?**

An image sprite is a single asset located within an image sprite sheet.
An image sprite sheet is an image file that contains more than one asset that can be extracted from it.

For example:

[![a basic image sprite sheet][1]][1]

The image above is an image sprite sheet, and each one of those stars is a sprite within the sprite sheet. These sprite sheets are useful because they improve performance by reducing the number of HTTP requests a browser might have to make.

So how do you implement one? Here's some example code.

**HTML**

    <div class="icon icon1"></div>
    <div class="icon icon2"></div>
    <div class="icon icon3"></div>

**CSS**

<!-- language: lang-css -->

    .icon {
        background: url(“icons-sprite.png”);
        display: inline-block;
        height: 20px;
        width: 20px;
    }
    .icon1 {
          background-position: 0px 0px;
    }
    .icon2 {
          background-position: -20px 0px;
    }
    .icon3 {
          background-position: -40px 0px;
    }

By using setting the sprite's width and height and by using the background-position property in CSS (with an x and y value) you can easily extract sprites from a sprite sheet using CSS.

  [1]: http://i.stack.imgur.com/XuyVW.png

