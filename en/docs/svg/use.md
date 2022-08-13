---
title: "use"
slug: "use"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| --------- | ------- |
| x         | x-axis coordinate of the upper left corner |
| y         | y-axis coordinate of the upper left corner |
| width     | width of the `<use>` element |
| height    | height of the `<use>` element
| xlink:href| resource identifier (refers to the ID of another element) SVG 2 proposes to deprecate this and replace it with a simple href attribute

Details can be found in the [W3C Recommendation for SVG](https://www.w3.org/TR/SVG11/struct.html#UseElement) as well as the new [Candidate Recommendation for SVG2](https://svgwg.org/svg2-draft/struct.html#UseElement) 

## Using an Icon
The `<use>` element is often used for reuseable icons, in collaboration with the `<symbol>` element. That looks like this:

```
<svg>
    <symbol viewBox="0 0 16 16" id="icon-star">
        <path d="M16 6.216l-6.095-.02L7.98.38 6.095 6.196 0 6.215h.02l4.912 3.57-1.904 5.834h.02l4.972-3.59 4.932 3.59-1.904-5.815L16 6.215" />
    </symbol>
</svg>
```
And the `<use>` element:
```
<svg>
    <use xlink:href="#icon-star"/>
</svg>
```

The `<use>` element copies the `<symbol>` and displays it. You can also override the styles on the `<symbol>` on individual `<use>` elements, e.g.

```
<style>
    .red {
        fill: red;
    }
</style>

<svg>
    <use class="red" xlink:href="#icon-star"/>
</svg>
```

