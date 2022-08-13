---
title: "Mixins"
slug: "mixins"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Introduction
Mixins are similar to defining and calling functions. Say, if we need to create a repetitive style, mixins are handy. Mixins may or may not take parameters. For e.g.:

    .default-round-borders {
              border-radius: 5px;
         -moz-border-radius: 5px;
      -webkit-border-radius: 5px;
    }

    .round-borders (@radius) {
              border-radius: @radius;
         -moz-border-radius: @radius;
      -webkit-border-radius: @radius;
    }

We have two types of declarations above. One takes in a parameter and the other doesn't. Let's see how this is being used somewhere:

    @sky-blue: #87ceeb;
    @dark-sky-blue: #baffff;

    #header {
      background: @sky-blue;
      .default-round-borders;
    }
    
    .btn {
      background: @dark-sky-blue;
      .round-borders(3px);
    }

The above code, compiled all together will give an output like this:

    #header {
      background: #87ceeb;
              border-radius: 5px;
         -moz-border-radius: 5px;
      -webkit-border-radius: 5px;
    }
    .btn {
      background: #baffff;
              border-radius: 3px;
         -moz-border-radius: 3px;
      -webkit-border-radius: 3px;
    }

## Prevent a mixin definition from appearing in the compiled CSS file
    .default-settings() {
      padding: 4px;
      margin: 4px;
      font-size: 16px;
      border: 1px solid gray;
    }
    
    #demo {
      .default-settings;
    }

The above example when compiled would only produce the following output. The `.default-settings()` mixin definition would not be output in the compiled CSS file because parenthesis is added after it. 

    #demo {
      padding: 4px;
      margin: 4px;
      font-size: 16px;
      border: 1px solid gray;
    }


If these parenthesis are not attached, the Less compiler will treat the mixin definition also as CSS selector and so will print it also in the output CSS file.

## Add !important to every property in a mixin without manually typing it
When we attach the `!important` keyword to a mixin call, the Less compiler will automatically add the `!important` to all properties that are present within the mixin.

For example, consider the below mixin:

<!-- language: lang-css -->

    .set-default-props() {
      margin: 4px;
      padding: 4px;
      border: 1px solid black;
      font-weight: normal;
    }

When the `!important` is attached to the mixin call like below,

<!-- language: lang-css -->

    #demo {
      .set-default-props() !important;
    }

the compiled CSS would look as follows:

<!-- language: lang-css -->

    #demo {
      margin: 4px !important;
      padding: 4px !important;
      border: 1px solid black !important;
      font-weight: normal !important;
    }

> **Note:** Usage of `!important` is considered as bad practice and must be used only as the last resort.

