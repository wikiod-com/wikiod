---
title: "Label Element"
slug: "label-element"
draft: false
images: []
weight: 9922
type: docs
toc: true
---

## Syntax
- `<label>Example <input type="radio" name="r"></label>` // Wrapping a control Element
- `<label for="rad1">Example</label> <input id="rad1" type="radio" name="r">`   // Using `for` attribute

## Parameters
| Attributes | Description |
| --------- | ------- |
| for       | Reference to the target ID Element. I.e: `for="surname"`
| form      | **HTML5**, **[Obsolete]**  Reference to the form containing the Target Element. Label elements are expected within a `<form>` Element. If the `form="someFormId"` is provided this allows you to place the Label anywhere in the document.

## About Label
The `<label>` element is used to reference a form action element.  
In the scope of **User Interface** it's used to ease the target / selection of elements like Type `radio` or `checkbox`. 

**`<label>` as wrapper**

It can enclose the desired action element

    <label>
        <input type="checkbox" name="Cats">
        I like Cats!
    </label>

(Clicking on the text the target `input` will toggle it's state / value)

**`<label>` as reference**

Using the `for` attribute you don't have to place the control element as descendant of `label` - but the `for` value must match it's ID 


    <input id="cats" type="checkbox" name="Cats">
    <label for="cats" >I like Cats!</label>

**Note**  
Don't use more than one Control Element within a `<label>` element



## Basic Use
Simple form with labels...

    <form action="/login" method="POST">
        
        <label for="username">Username:</label>
        <input id="username" type="text" name="username" />

        <label for="pass">Password:</label>
        <input id="pass" type="password" name="pass" />

        <input type="submit" name="submit" />

    </form>


<!-- if version [gte 5] -->
    <form id="my-form" action="/login" method="POST">
        
        <input id="username" type="text" name="username" />

        <label for="pass">Password:</label>
        <input id="pass" type="password" name="pass" />

        <input type="submit" name="submit" />

    </form>

    <label for="username" form="my-form">Username:</label>
<!-- end version if -->

