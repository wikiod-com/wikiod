---
title: "The SVG element"
slug: "the-svg-element"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## preserveAspectRatio
`preserveAspectRatio` is an attribute that indicates whether the image should be scaled uniformly. This attribute only works if the `<svg>` element also has a `viewBox`.

The default value is `xMidYMid`, which maintains the aspect ratio and centers the path inside the SVG container:

```
<!-- when not included `preserveAspectRatio` defaults to `xMidYMid` -->
<svg viewBox="0 0 16 16" height="60" width="120">
    <path d="M16 6.216l-6.095-.02L7.98.38 6.095 6.196 0 6.215h.02l4.912 3.57-1.904 5.834h.02l4.972-3.59 4.932 3.59-1.904-5.815L16 6.215" />
</svg>
```

[![star that is centered within the container][2]][2]

When `preserveAspectRatio` is set to `none`, the icon stretches to fit the box:

```
<svg viewBox="0 0 16 16" height="60" width="120" preserveAspectRatio="none">
    <path d="M16 6.216l-6.095-.02L7.98.38 6.095 6.196 0 6.215h.02l4.912 3.57-1.904 5.834h.02l4.972-3.59 4.932 3.59-1.904-5.815L16 6.215" />
</svg>
```

[![star that has been stretched][1]][1]

There are many other values for `preserveAspectRatio`, but these two are by far the most common.


  [1]: http://i.stack.imgur.com/mVsGs.png
  [2]: http://i.stack.imgur.com/sl0yO.png

## viewBox
The viewBox attribute defines the coordinate system for an `<svg>` element. This allows you to easily change the size and relative proportion of an SVG graphic without having to adjust the position and dimensions of every individual drawn element.

```
<!-- stretches a small icon to 60px square -->
<svg viewBox="0 0 16 16" height="60px" width="60px">
    <path d="M16 6.216l-6.095-.02L7.98.38 6.095 6.196 0 6.215h.02l4.912 3.57-1.904 5.834h.02l4.972-3.59 4.932 3.59-1.904-5.815L16 6.215" />
</svg>
```

That code looks like this:

[![large 60px star icon][1]][1]

Without the viewBox, it looks like this:

[![small 16px star icon][2]][2]


  [1]: http://i.stack.imgur.com/dZkWJ.png
  [2]: http://i.stack.imgur.com/AdeZu.png

## preserveAspectRatio - meet and slice attributes
The `preserveAspectRatio` attribute has an optional parameter: `meet` | `slice`. The default behavior is `meet` which stretches the content in both the x and y dimension until it fills ***either*** the width or height of the viewBox. The alternative - `slice` preserves the aspect ratio of the content but scales up the graphic until it fills ***both*** the width and height of the viewbox (clipping the content that overflows the viewBox).

This is the example using `slice`

    <svg viewBox="0 0 16 16" height="60px" width="120px" preserveAspectRatio="xMinYMin slice">
    <path d="M16 6.216l-6.095-.02L7.98.38 6.095 6.196 0 6.215h.02l4.912 3.57-1.904 5.834h.02l4.972-3.59 4.932 3.59-1.904-5.815L16 6.215" />
</svg>

which renders as:

[![enter image description here][1]][1]

and the same example using `meet`

        <svg viewBox="0 0 16 16" height="60px" width="120px" preserveAspectRatio="xMinYMin meet">
        <path d="M16 6.216l-6.095-.02L7.98.38 6.095 6.196 0 6.215h.02l4.912 3.57-1.904 5.834h.02l4.972-3.59 4.932 3.59-1.904-5.815L16 6.215" />
    </svg>

which renders as:

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/B8d05.png
  [2]: http://i.stack.imgur.com/PGe8D.png

