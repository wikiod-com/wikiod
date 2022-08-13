---
title: "Color Operation Functions"
slug: "color-operation-functions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - contrast(&lt;reference-color>, &lt;output-for-light-refcolor>, &lt;output-for-dark-refcolor>, &lt;threshold>)
 - lighten(&lt;reference-color>, &lt;amount>, &lt;method>)
 - darken(&lt;reference-color>, &lt;amount>, &lt;method>)

## Parameters

| Parameter | Details |
| ------ | ------ |
| reference-color   | The color based on which the color operation should be performed.   |
| output-for-light-ref-color | The color value that should be output when reference color is a dark color. This is optional and default value is white. |
| output-for-dark-ref-color | The color value that should be output when reference color is a light color. This is optional and default value is black. |
| threshold | This is a percentage value which defines when the reference color is considered as a dark color and when it is considered as a light color. Color comparison is done using gamma-corrected luma values. This is optional and the  default value is 43% |
| amount | This is a percentage value which specifies the amount by which the reference color should be darkened or lightened. |
| method | This can either be `absolute` or `relative` and defines whether the adjustment should be relative to the current value or not. It is an optional field and the default value is absolute.

Contrast
=========

In the contrast function, the output-for-dark-ref-color and output-for-light-ref-color can be provided in any order. The function automatically identifies which is the dark color (to be used when ref color is light) and which is light color (to be used when ref color is dark) based on their own luma values.

## Setting text color depending on the darkness or lightness of background color
    #demo-element {
      background: @theme-color;
      color: contrast(@theme-color, black, white, 50%);
    }
    
    @theme-color: red;

The above example will set the text color of the element as `white` if the `background-color` is dark and vice-versa. This is achieved using the [`contrast()`](http://lesscss.org/functions/#color-operations-contrast) color operation function. 

The contrast function accepts four parameters where the first one is the reference color based on which the output should be provided. A dark color that should be output when the reference color is light and a light that should be output when reference color is dark . Threshold is a percentage which defines when a color is considered as a light color and when dark.

A demo of this example can be found [here](http://codepen.io/hari_shanx/pen/oLrokR).

## Set a darker or lighter shade of the background color for another property
Darken
=======

    #demo {
      @refcolor: #f0b9b8;
      background: @refcolor;
      border: 1px solid darken(@refcolor, 25%);
    }

The above code makes use of the `darken()` function to set the border color as a shade that is 25% darker than the reference color (which is also the background color).

Less compiler cannot read the value assigned to one property and use it for another and so a separate variable should be defined. This variable will be directly assigned to the background color whereas for the border color, the `darken` function is used to get a darker shade.

----

Lighten
=========

    #demo {
      @refcolor: #f0b9b8;
      background: @refcolor;
      box-shadow: 2px 2px 0px lighten(@refcolor, 5%);
    }

The above code makes use of the `lighten()` function to set the shadow color as a shade that is 5% lighter than the reference color (which is also the background color).

## Changing opacity
It is possible to change the opacity of a color with `fade()` function.

`fade()` takes 2 parameters:
- a color
- opacity (in %)

---

**Example:**

    @elegant: #eeffgg;
    
    .light-elegant {
        background-color: fade(@elegant, 20%);
    }

<!-- Code break -->

    <div class="light-elegant">
        I have a 20% elegant background!
    </div>

