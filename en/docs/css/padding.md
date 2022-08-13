---
title: "Padding"
slug: "padding"
draft: false
images: []
weight: 9894
type: docs
toc: true
---

## Syntax
- padding: *length*|initial|inherit|unset;
- padding-top: *length*|initial|inherit|unset;
- padding-right: *length*|initial|inherit|unset;
- padding-bottom: *length*|initial|inherit|unset;
- padding-left: *length*|initial|inherit|unset;


> [The padding property sets the padding space on all sides of an
> element. The padding area is the space between the content of the
> element and its border. **Negative values are not allowed**.][1]


  [1]: https://developer.mozilla.org/en/docs/Web/CSS/padding MDN

Also see this [question][1], ["Why does CSS not support negative padding?"][1] and his answers.

Also please consider https://www.wikiod.com/css/the-box-model when using padding. Depending on the box-sizing value, padding on an element can either add to the previously defined height/width of an element or not.

Related Properties:

[margin][2]


  [1]: http://stackoverflow.com/questions/4973988/why-does-css-not-support-negative-padding
  [2]: https://www.wikiod.com/css/margins



Padding on inline elements will only apply to the left and right of the element, and not the top and bottom, due to the inherent display properties of inline elements. 

## Padding Shorthand
The padding property sets the padding space on all sides of an element. The padding area is the space between the content of the element and its border. Negative values are not allowed.

To save adding padding to each side individually (using `padding-top`, `padding-left` etc) can you write it as a shorthand, as below:

**Four values**:

```
<style>
    .myDiv {
        padding: 25px 50px 75px 100px; /* top right bottom left; */
    }
</style>
<div class="myDiv"></div>
```
[![enter image description here][1]][1]

**Three values**:

```
<style>
    .myDiv {
        padding: 25px 50px 75px; /* top left/right bottom */
    }
</style>
<div class="myDiv"></div>
```
[![enter image description here][2]][2]

**Two values**:

```
<style>
    .myDiv {
        padding: 25px 50px; /* top/bottom left/right */
    }
</style>
<div class="myDiv"></div>
```
[![enter image description here][3]][3]

**One value**: 

```
<style>
    .myDiv {
        padding: 25px; /* top/right/bottom/left */
    }
</style>
<div class="myDiv"></div>
```
[![enter image description here][4]][4]


  [1]: http://i.stack.imgur.com/xWS9v.png
  [2]: http://i.stack.imgur.com/Qrs3R.png
  [3]: http://i.stack.imgur.com/LiW8C.png
  [4]: http://i.stack.imgur.com/GdRZW.png

## Padding on a given side
The padding property sets the padding space on all sides of an element. The padding area is the space between the content of the element and its border. Negative values are not allowed.

You can specify a side individually:

 - `padding-top`
 - `padding-right`
 - `padding-bottom`
 - `padding-left`

The following code would add a padding of `5px` to the top of the div:
    
    <style>
    .myClass {
        padding-top: 5px;
    }
    </style>

    <div class="myClass"></div>

