---
title: "Grid System"
slug: "grid-system"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Bootstrap Grid System
<br>**Bootstrap uses a Grid System having rows and columns**

In the Grid system, you are using a *row* class  to create a horizontal box with a total of 12 columns of size 1 unit each for different screen size vertically. If you do not want to use all 12 columns individually, you can group the columns together to create wider columns. <br/>

Example :
if you want to make a row of 3 columns - you have a div with class="row" (i.e one horizontal box) and 3 columns( class = col.xs.xx) each of size 3, size 2, size 7 ( 3+2+7 =12) for xs = extra small screen size of size

<div class="row row-list">
    <div class="col-xs-3"><h3>title</h3></div>
    <div class="col-xs-2 container-img"><img src=""> </div>
    <div class="col-xs-7 container-paragraph"><p>lorem ipsum</p></div>
</div>  

[![Grid bootstrap][1]][1]
<br><br>
**Grid Classes**

The Bootstrap grid system has four classes for responsive design like this:

    xs (for phones)
    sm (for tablets)
    md (for desktops)
    lg (for larger desktops)
<br><br>
**How to use?**<br><br>
For basic example 4 columns

    <div class="row">
      <div class="col-sm-4">Your Div Content</div>
      <div class="col-sm-4">Your Div Content</div>
      <div class="col-sm-4">Your Div Content</div>
    </div>
<br>Example 4 columns<br>
[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/onC5e.png
  [2]: http://i.stack.imgur.com/eX62T.png

