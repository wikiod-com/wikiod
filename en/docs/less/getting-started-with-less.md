---
title: "Getting started with less"
slug: "getting-started-with-less"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Less has been one of the most popular CSS Pre-processors, and has also been widely deployed in numerous front-end frameworks like Bootstrap, Foundation, etc. The Less Compiler is a JavaScript based compiler, which can be obtained from a Content Delivery Network:

<!-- language: lang-html -->

    <script src="//cdnjs.cloudflare.com/ajax/libs/less.js/2.5.1/less.min.js"></script>

You need to add your Less document **before** the JavaScript compiler is loaded, using `<link />` tag. The Less stylesheet along with the compiler looks like this:

<!-- language: lang-html -->

    <link rel="stylesheet/less" type="text/css" href="main.less" />
    <script src="//cdnjs.cloudflare.com/ajax/libs/less.js/2.5.1/less.min.js"></script>

**Note:** Compiling Less in the client-side (or in the browser) is generally not recommended. It should used only for development or when using dynamic settings that make it not possible to compile server-side.

## Sample Less Syntax
The following example is a sample Less file which shows how variables are declared and used, how mixins are defined and called in Less.

<!-- language: lang-css -->

    /* Variables */
    @color-base: #87ceeb;

    /* Simple mixin to set border */

    .set-border(@width; @style; @color) {
      border: @width @style darken(@color, 10%);
    }

    /* Main CSS */
    .class1 {
      background-color: @color-base;
      .set-border(1px; solid; @color-base);
      .class2 {
        background-color: #fff;
        color: @color-base;
        .set-border(1px; solid; #fff);
      }
    }

The above code when compiled will produce the following CSS: (comments are stripped for brevity)

<!-- language: lang-css -->

    .class1 {
      background-color: #87ceeb;
      border: 1px solid #5bbce4;
    }
    .class1 .class2 {
      background-color: #fff;
      color: #87ceeb;
      border: 1px solid #e6e6e6;
    }

## Compiling a Less file from the command line
    lessc [options] <source> [destination]

The above command is used to compile Less files in the command line. Options are the various settings that the compiler should use either during compilation or after compilation. Options include `-x` or `--compress` for compressing or minifying the output CSS file, `-sm=on` or `--strict-math=on` for applying math operations only on values enclosed within parenthesis etc. The next comes the path of the source Less file that has to be compiled. Destination is the path and name of the output file. If this is not provided the output is printed out in the command line window itself.

Consider the below Less code

<!-- language: lang-css -->

    /* Filename: test.less */
    #demo {
     color: @color;
     background: beige;
     width: 100% / 4;
    }
    @color: red;

**Print compiled CSS in Command window:**

When the following command is executed in the command line, the test.less file would be compiled and the output will be printed directly on the command window as no destination path is provided.

    lessc test.less

Output:

<!-- language: lang-css -->

    #demo {
      color: red;
      background: beige;
      width: 25%;
    }

**Create a CSS file and write compiled output to the file:**

The same file when compiled with the below statement will create a file named test.css in the same path as the test.less file and print/write the output to that CSS file.

    lessc test.less > test.css

**Create a CSS file and minify it:**

The below command will print/write the output to a CSS file and also compress it at the end.

    lessc -x test.less > test.css

Output:

<!-- language: lang-css -->

    #demo{color:red;background:beige;width:25%}

**With Strict Math option enabled:**

When the strict match option is enabled, the output will be as follows because the values for `width` is not enclosed within braces.

    lessc -sm=on test.less > test.css

Output:

<!-- language: lang-css -->

    #demo {
      color: red;
      background: beige;
      width: 100% / 4;
    }

## Nesting in Less
In Less you can write much more simple CSS rules and also keep them well formatted, so instead of writing this code: 

**CSS**

<!-- language: lang-css -->

    .item {
      border: 1px solid;
      padding: 4px;
    }
    .item .content, .item .image {
      float: left;
    }
    .item .content {
      font-size: 12px;
    }
    .item .image {
      width: 300px;
    }

you can just write this:

**Less**

<!-- language: lang-css -->

    .item {
      border: 1px solid;
      padding: 4px;
      .content, .image {
        float: left;
      }
      .content {
        font-size: 12px;
      }
      .image {
        width: 300px;
      }
    }

and Less will compile that code into the normal CSS we all know.

## Joining Files - Imports
The `@import` statement allows you to insert CSS/Less code from another file into your own CSS/Less file.

<!-- language: lang-css -->

    .foo {
      background: #900;
    }
    @import "my-other-css-file.css";
    @import "my-other-less-file.less";

