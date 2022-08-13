---
title: "PorterDuff Mode"
slug: "porterduff-mode"
draft: false
images: []
weight: 9660
type: docs
toc: true
---

PorterDuff is described as a way of combining images as if they were "irregular shaped pieces of cardboard" overlayed on each other, as well as a scheme for blending the overlapping parts

"Porter Duff" in itself is an [alpha compositing technique][1] named after a [paper by Thomas Porter and Tom Duff.][2]

To summarize, the technique takes two images with alpha channel and generates the output image by combining pixels values of two images. The various combining modes result in different output image. For example, in following image, blue shape (source, existing pixels) is combined with Yellow shape (destination, new pixels) in different modes:

[![Porter duff modes][3]][3]


  [1]: https://en.wikipedia.org/wiki/Alpha_compositing
  [2]: http://keithp.com/~keithp/porterduff/p253-porter.pdf
  [3]: http://i.stack.imgur.com/qjlRN.png

## Creating a PorterDuff ColorFilter
[`PorterDuff.Mode`][1] is used to create a [`PorterDuffColorFilter`][2]. A color filter modifies the color of each pixel of a visual resource.

    ColorFilter filter = new PorterDuffColorFilter(Color.BLUE, PorterDuff.Mode.SRC_IN);

The above filter will tint the non-transparent pixels to blue color.

The color filter can be applied to a [`Drawable`][3]:

    drawable.setColorFilter(filter);

It can be applied to an [`ImageView`][5]:

    imageView.setColorFilter(filter);

Also, it can be applied to a [`Paint`][4], so that the color that is drawn using that paint, is modified by the filter:

    paint.setColorFilter(filter);

  [1]: http://developer.android.com/reference/android/graphics/PorterDuff.Mode.html
  [2]: http://developer.android.com/reference/android/graphics/PorterDuffColorFilter.html
  [3]: http://developer.android.com/reference/android/graphics/drawable/Drawable.html#setColorFilter(android.graphics.ColorFilter)
  [4]: http://developer.android.com/reference/android/graphics/Paint.html#setColorFilter(android.graphics.ColorFilter)
  [5]: http://developer.android.com/reference/android/widget/ImageView.html#setColorFilter(int)

## Creating a PorterDuff XferMode
An [`Xfermode`][1] (think "transfer" mode) works as a transfer step in drawing pipeline. When an `Xfermode` is applied to a `Paint`, the pixels drawn with the paint are combined with underlying pixels (already drawn) as per the mode:

    paint.setColor(Color.BLUE);
    paint.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.SRC_IN));

Now we have a blue tint paint. Any shape drawn will tint the already existing, non-transparent pixels blue in the area of the shape.

  [1]: http://developer.android.com/reference/android/graphics/Xfermode.html

## Apply a radial mask (vignette) to a bitmap using PorterDuffXfermode
    /**
     * Apply a radial mask (vignette, i.e. fading to black at the borders) to a bitmap
     * @param imageToApplyMaskTo Bitmap to modify
     */
    public static void radialMask(final Bitmap imageToApplyMaskTo) {
        Canvas canvas = new Canvas(imageToApplyMaskTo);

        final float centerX = imageToApplyMaskTo.getWidth() * 0.5f;
        final float centerY = imageToApplyMaskTo.getHeight() * 0.5f;
        final float radius = imageToApplyMaskTo.getHeight() * 0.7f;

        RadialGradient gradient = new RadialGradient(centerX, centerY, radius,
                0x00000000, 0xFF000000, android.graphics.Shader.TileMode.CLAMP);

        Paint p = new Paint();
        p.setShader(gradient);
        p.setColor(0xFF000000);
        p.setXfermode(new PorterDuffXfermode(PorterDuff.Mode.DST_OUT));
        canvas.drawRect(0, 0, imageToApplyMaskTo.getWidth(), imageToApplyMaskTo.getHeight(), p);
    }

