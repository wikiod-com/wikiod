---
title: "Conditional Rendering"
slug: "conditional-rendering"
draft: false
images: []
weight: 9234
type: docs
toc: true
---

## Syntax
- `<element v-if="condition"></element>` //v-if
- `<element v-if="condition"></element><element v-else="condition"></element>` //v-if | v-else
- `<template v-if="condition">...</template>` //templated v-if
- `<element v-show="condition"></element>` //v-show

It is very important to remember the difference between `v-if` and `v-show`. While their uses are almost identical, an element bound to `v-if` *will only render into the DOM* when it's condition is `true` for the **first time**. When using the `v-show` directive, *all elements **are** rendered into the DOM* but are hidden using the `display` style if the condition is `false`!

## Overview
In Vue.js, conditional rendering is achieved by using a set of directives on elements in the template.

`v-if`
-
Element displays normally when condition is `true`. When the condition is `false`, only *partial* compilation occurs and the element isn't rendered into the DOM until the condition becomes `true`.

`v-else`
-
Does not accept a condition, but rather renders the element if the previous element's `v-if` condition is `false`. Can only be used after an element with the `v-if` directive.

`v-show`
-
Behaves similarly to `v-if`, however, the element will *always* be rendered into the DOM, even when the condition is `false`. If the condition is `false`, this directive will simply set the element's `display` style to `none`.

## v-if / v-else
Assuming we have a `Vue.js` instance defined as:

<!-- language: lang-js -->
    var vm = new Vue({
        el: '#example',
        data: {
            a: true,
            b: false
        }
    });

You can conditionally render any html element by including the v-if directive; the element that contains v-if will only render if the condition evaluates to true:

<!-- language: lang-html -->
    <!-- will render 'The condition is true' into the DOM -->
    <div id="example">
        <h1 v-if="a">The condition is true</h1>
    </div>

The `<h1>` element will render in this case, because the variable 'a' is true. v-if can be used with any expression, computed property, or function that evaluates to a boolean:


<!-- language: lang-html -->
    <div v-if="0 === 1">                  false; won't render</div>
    <div v-if="typeof(5) === 'number'">   true; will render</div>
    
 
You can use a `template` element to group multiple elements together for a single condition:

<!-- language: lang-html -->
    <!-- in this case, nothing will be rendered except for the containing 'div' -->
    <div id="example">
        <template v-if="b">
            <h1>Heading</h1>
            <p>Paragraph 1</p>
            <p>Paragraph 2</p>
        </template>
    </div>

When using `v-if`, you also have the option of integrating a counter condition with the `v-else` directive. The content contained inside the element will only be displayed if the condition of the previous v-if was false. Note that this means that an element with v-else must appear immediately after an element with v-if.

<!-- language: lang-html -->
    <!-- will render only 'ELSE' -->
    <div id="example">
        <h1 v-if="b">IF</h1>
        <h1 v-else="a">ELSE</h1>
    </div>

Just as with v-if, with v-else you can group multiple html elements together within a `<template>`: 

<!-- language: lang-html -->
    <div v-if="'a' === 'b'"> This will never be rendered. </div>
    <template v-else>
        <ul>
          <li> You can also use templates with v-else. </li>
          <li> All of the content within the template </li>
          <li> will be rendered. </li>
        </ul>
    </template>


## v-show
The use of the `v-show` directive is almost identical to that of `v-if`. The only differences are that `v-show` *does not* support the `<template>` syntax, and there is no "alternative" condition.

<!-- language: lang-js -->
    var vm = new Vue({
        el: '#example',
        data: {
            a: true
        }
    });

The basic use is as follows...

<!-- language: lang-html -->
    <!-- will render 'Condition met' -->
    <div id="example">
        <h1 v-show="a">Condition met</h1>
    </div>

While `v-show` does not support the `v-else` directive to define "alternative" conditions, this can be accomplished by negating the previous one...

<!-- language: lang-html -->
    <!-- will render 'This is shown' -->
    <div id="example">
        <h1 v-show="!a">This is hidden</h1>
        <h1 v-show="a">This is shown</h1>
    </div>

