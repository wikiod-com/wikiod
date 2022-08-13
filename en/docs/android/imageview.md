---
title: "ImageView"
slug: "imageview"
draft: false
images: []
weight: 9904
type: docs
toc: true
---

ImageView (`android.widget.ImageView`) is a View for displaying and manipulating image resources, such as Drawables and Bitmaps.

Some effects, discussed in this topic, can be applied to the image. The image source can be set in XML file (`layout` folder) or by programatically in Java code.

## Syntax
- The method `setImageResource(int resId)` sets a drawable as the content of this `ImageView`.
- **Usage:** `imageView.setImageResource(R.drawable.anyImage)`


## Parameters

| **Parameter**| **Description**|
| ------ | ------ |
| `resId`| your Image file name in the `res` folder (usually in `drawable` folder)|


## Set tint
Set a tinting color for the image. By default, the tint will blend using `SRC_ATOP` mode.   

set tint using XML attribute:

    android:tint="#009c38"

**Note:** Must be a color value, in the form of `"#rgb"`, `"#argb"`, `"#rrggbb"`, or `"#aarrggbb"`.  

set tint programmatically:

    imgExample.setColorFilter(Color.argb(255, 0, 156, 38));
and you can clear this color filter:

    imgExample.clearColorFilter();
**Example:**

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/0dT7q.png

## Set alpha
"alpha" is used to specify the opacity for an image.

set alpha using XML attribute:
 
    android:alpha="0.5"  
**Note:** takes float value from 0 (transparent) to 1 (fully visible)  

set alpha programmatically:  

    imgExample.setAlpha(0.5f);
 [![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/Mtvee.png

## Set Scale Type
Controls how the image should be resized or moved to match the size of `ImageView`.  

XML attribute:

    android:scaleType="..."
i will illustrate different scale types with a square `ImageView` which has a black background and we want to display a rectangular drawable in white background in `ImageView`.  

     <ImageView
      android:id="@+id/imgExample"
      android:layout_width="200dp"
      android:layout_height="200dp"
      android:background="#000" 
      android:src="@drawable/android2"
      android:scaleType="..."/>

scaleType must be one of the following values:

 1. `center`:Center the image in the view, but perform no scaling.




[![enter image description here][1]][1]

 2. `centerCrop`: Scale the image uniformly (maintain the image's aspect ratio) so both dimensions (width and height) of the image will be equal to or larger than the corresponding dimension of the view (minus padding). The image is then centered in the view. 

[![enter image description here][2]][2]

 3. `centerInside`: Scale the image uniformly (maintain the image's aspect ratio) so that both dimensions (width and height) of the image will be equal to or less than the corresponding dimension of the view (minus padding). The image is then centered in the view. 

[![enter image description here][3]][3]

 4. `matrix` : Scale using the image matrix when drawing.

[![enter image description here][4]][4]

 5. `fitXY`: Scale the image using [FILL][5]. 

[![enter image description here][6]][6]

 6. `fitStart`: Scale the image using [START][7]. 

[![enter image description here][8]][8]

 7. `fitCenter`: Scale the image using [CENTER][9]. 

[![enter image description here][10]][10]

 8. `fitEnd`: Scale the image using [END][11]. 

[![enter image description here][12]][12]


  [1]: http://i.stack.imgur.com/dP3mv.png
  [2]: http://i.stack.imgur.com/ZRcGg.png
  [3]: http://i.stack.imgur.com/Xtlr4.png
  [4]: http://i.stack.imgur.com/tLTMx.png
  [5]: https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#FILL
  [6]: http://i.stack.imgur.com/eVrlt.png
  [7]: https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#START
  [8]: http://i.stack.imgur.com/9DRc1.png
  [9]: https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#CENTER
  [10]: http://i.stack.imgur.com/XqB7I.png
  [11]: https://developer.android.com/reference/android/graphics/Matrix.ScaleToFit.html#END
  [12]: http://i.stack.imgur.com/Idwdh.png

## Set Image Resource
    <ImageView
     android:id="@+id/imgExample"
     android:layout_width="wrap_content"
     android:layout_height="wrap_content"
     ...
     />

set a drawable as content of `ImageView` using XML attribute:  

    android:src="@drawable/android2"  
set a drawable programmatically:  

     ImageView imgExample = (ImageView) findViewById(R.id.imgExample);
     imgExample.setImageResource(R.drawable.android2);

## ImageView ScaleType - Center
The image contained in the ImageView may not fit the exact size given to the container. In that case, the framework allows you to resize the image in a number of ways.

**Center**

        <ImageView android:layout_width="20dp"
               android:layout_height="20dp"
               android:src="@mipmap/ic_launcher"
               android:id="@+id/imageView"
               android:scaleType="center"
               android:background="@android:color/holo_orange_light"/>

This will not resize the image, and it will center it inside the container
*(Orange = container)*

[![Center][1]][1]

In case that the ImageView is smaller than the image, the image will not be resized and you will only be able to see a part of it

[![Center image bigger than imageView][2]][2]

**strong text**


  [1]: http://i.stack.imgur.com/1ORfG.png
  [2]: http://i.stack.imgur.com/b7hZS.png

## ImageView ScaleType - CenterCrop
Scale the image uniformly (maintain the image's aspect ratio) so that both dimensions (width and height) of the image will be equal to or larger than the corresponding dimension of the view (minus padding). 

[Official Docs][1]

When the image matches the proportions of the container:

[![enter image description here][2]][2]

When the image is wider than the container it will expand it to the bigger size (in this case height) and adjust the width of the image without changing it's proportions, causing it to crop.

[![enter image description here][3]][3]


  [1]: https://developer.android.com/reference/android/widget/ImageView.ScaleType.html
  [2]: http://i.stack.imgur.com/1se4G.png
  [3]: http://i.stack.imgur.com/81ohC.png

## ImageView ScaleType - CenterInside
Scale the image uniformly (maintain the image's aspect ratio) so that both dimensions (width and height) of the image will be equal to or less than the corresponding dimension of the view (minus padding). 

[Official Docs][1]

It will center the image and resize it to the smaller size, if both container sizes are bigger it will act the same as center.

[![center inside 1][2]][2]

But if one of the sizes are small, it will fit to that size.

[![center inside 2][3]][3]


  [1]: https://developer.android.com/reference/android/widget/ImageView.ScaleType.html
  [2]: http://i.stack.imgur.com/osAmy.png
  [3]: http://i.stack.imgur.com/R6bh5.png

## ImageView ScaleType - FitStart and FitEnd
Scale the image using START. 

Scale the image using END. 

[Official Docs][1]

**FitStart**

This will fit to the smallest size of the container, and it will align it to the start.

    <ImageView android:layout_width="200dp"
               android:layout_height="200dp"
               android:src="@mipmap/ic_launcher"
               android:id="@+id/imageView"
               android:scaleType="fitStart"
               android:layout_gravity="center"
               android:background="@android:color/holo_orange_light"/>

[![fit to top][2]][2]

[![fit to left][3]][3]

**FitEnd**

This will fit to the smallest size of the container, and it will align it to the end.

    <ImageView android:layout_width="200dp"
               android:layout_height="100dp"
               android:src="@mipmap/ic_launcher"
               android:id="@+id/imageView"
               android:scaleType="fitEnd"
               android:layout_gravity="center"
               android:background="@android:color/holo_orange_light"/>
               
 [![fit to bottom][4]][4]

[![fit to right][5]][5]


  [1]: https://developer.android.com/reference/android/widget/ImageView.ScaleType.html
  [2]: http://i.stack.imgur.com/a5bmE.png
  [3]: http://i.stack.imgur.com/OfVmD.png
  [4]: http://i.stack.imgur.com/NwOJG.png
  [5]: http://i.stack.imgur.com/CuXz3.png

## ImageView ScaleType - FitCenter
Scale the image using CENTER. 

[Official Docs][1]

This expands the image to try to match the container and it will align it to the center, it will fit to the smaller size.

Bigger height ( fit to width )

[![enter image description here][2]][2]

Same width and height.

[![enter image description here][3]][3]


  [1]: https://developer.android.com/reference/android/widget/ImageView.ScaleType.html
  [2]: http://i.stack.imgur.com/MUZRF.png
  [3]: http://i.stack.imgur.com/IZfTx.png

## ImageView ScaleType - FitXy
Scale the image using FILL. 

[Official Docs][1]

    <ImageView android:layout_width="100dp"
               android:layout_height="200dp"
               android:src="@mipmap/ic_launcher"
               android:id="@+id/imageView"
               android:scaleType="fitXY"
               android:layout_gravity="center"
               android:background="@android:color/holo_orange_light"/>

[![enter image description here][2]][2]

[![enter image description here][3]][3]

[![enter image description here][4]][4]


  [1]: https://developer.android.com/reference/android/widget/ImageView.ScaleType.html
  [2]: http://i.stack.imgur.com/kw2Nj.png
  [3]: http://i.stack.imgur.com/lLDGC.png
  [4]: http://i.stack.imgur.com/ou0iQ.png

## MLRoundedImageView.java


