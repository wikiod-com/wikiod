---
title: "Supporting Screens With Different Resolutions, Sizes"
slug: "supporting-screens-with-different-resolutions-sizes"
draft: false
images: []
weight: 9880
type: docs
toc: true
---

 **Terms and concepts**
 

>  ## **Screen size**
> Actual physical size, measured as the screen's diagonal.  For simplicity, Android groups all actual screen sizes into  four generalized sizes: small, normal, large, and extra-large.
> ## **Screen density**
> The quantity of pixels within a physical area of the screen; usually referred to as dpi (dots per inch). For example, a "low" density screen has fewer pixels within a given physical area, compared to a "normal" or "high" density screen.  For simplicity, Android groups all actual screen densities into six generalized densities: low, medium, high, extra-high, extra-extra-high, and extra-extra-extra-high.
> ## **Orientation**
>  The orientation of the screen from the user's point of view. This is either landscape or portrait, meaning that the screen's aspect ratio is either wide or tall, respectively. Be aware that not only do different devices operate in different orientations by default, but the orientation can change at runtime when the user rotates the device.  Resolution  The total number of physical pixels on a screen. When adding support for multiple screens, applications do not work directly with resolution; applications should be concerned only with screen size and density, as specified by the generalized size and density groups. 
> Density-independent pixel (dp)  A virtual pixel unit that you should
> use when defining UI layout, to express layout dimensions or position
> in a density-independent way.  The density-independent pixel is
> equivalent to one physical pixel on a 160 dpi screen, which is the
> baseline density assumed by the system for a "medium" density screen.
> At runtime, the system transparently handles any scaling of the dp
> units, as necessary, based on the actual density of the screen in use.
> The conversion of dp units to screen pixels is simple: px = dp * (dpi
> / 160). For example, on a 240 dpi screen, 1 dp equals 1.5 physical
> pixels. You should always use dp units when defining your
> application's UI, to ensure proper display of your UI on screens with
> different densities.
>
> ----------
>
> ## **Units**
> 
>  - ## **px** 
>
>     Pixels - corresponds to actual pixels on the screen. 
>  - ## **in**
> 
>    Inches - based on the physical size of the screen. 1 Inch = 2.54 centimeters
>  - ## **mm**
> 
>     Millimeters - based on the physical size of the screen. 
>  - ## **pt**
>  
>    Points - 1/72 of an inch based on the physical size of the screen. 
>  - ## **dp or dip**
> 
>    Density-independent Pixels - an abstract unit that is based on the
> physical density of the screen. These units are relative to a 160 dpi
> screen, so one dp is one pixel on a 160 dpi screen. The ratio of
> dp-to-pixel will change with the screen density, but not necessarily
> in direct proportion. Note: The compiler accepts both "dip" and "dp",
> though "dp" is more consistent with "sp". 
>  - ## **sp**
>
>     Scale-independent Pixels - this is like the dp unit, but it is also scaled by the user's font size preference. It is recommend you use this unit when specifying
> font sizes, so they will be adjusted for both the screen density and
> user's preference. From Understanding Density Independence In Android:

----------

| Unit    | Description                        | Units Per Physical Inch | Density Independent | Same Physical Size On Every Screen | 
|---------|------------------------------------|-------------------------|---------------------|------------------------------------|
| px      | Pixels                             | Varies                  | No                  | No                                 | 
| in      | Inches                             | 1                       | Yes                 | Yes                                | 
| mm      | Millimeters                        | 25.4                    | Yes                 | Yes                                | 
| pt      | Points                             | 72                      | Yes                 | Yes                                | 
| dp      | Density Independent Pixels         | ~160                    | Yes                 | No                                 | 
| sp      | Scale Independent Pixels           | ~160                    | Yes                 | No                                 | 



  [1]: http://developer.android.com/guide/topics/resources/more-resources.html#Dimension

 
 References: 

 - https://developer.android.com/guide/practices/screens_support.html
 - http://developer.android.com/guide/topics/resources/more-resources.html




## Using configuration qualifiers
Android supports several configuration qualifiers that allow you to control how the system selects your alternative resources based on the characteristics of the current device screen. A configuration qualifier is a string that you can append to a resource directory in your Android project and specifies the configuration for which the resources inside are designed.

To use a configuration qualifier:

 1. Create a new directory in your project's res/ directory and name it using the format: `<resources_name>-<qualifier>`. `<resources_name>` is the standard resource name (such as drawable or layout).  
 2. `<qualifier>` is a configuration qualifier, specifying the screen configuration for which these resources are to be used (such as hdpi or xlarge).


For example, the following application resource directories provide different layout designs for different screen sizes and different drawables. Use the `mipmap/` folders for launcher icons.

    res/layout/my_layout.xml              // layout for normal screen size ("default")
    res/layout-large/my_layout.xml        // layout for large screen size
    res/layout-xlarge/my_layout.xml       // layout for extra-large screen size
    res/layout-xlarge-land/my_layout.xml  // layout for extra-large in landscape orientation
    
    res/drawable-mdpi/graphic.png         // bitmap for medium-density
    res/drawable-hdpi/graphic.png         // bitmap for high-density
    res/drawable-xhdpi/graphic.png        // bitmap for extra-high-density
    res/drawable-xxhdpi/graphic.png       // bitmap for extra-extra-high-density
    
    res/mipmap-mdpi/my_icon.png         // launcher icon for medium-density
    res/mipmap-hdpi/my_icon.png         // launcher icon for high-density
    res/mipmap-xhdpi/my_icon.png        // launcher icon for extra-high-density
    res/mipmap-xxhdpi/my_icon.png       // launcher icon for extra-extra-high-density
    res/mipmap-xxxhdpi/my_icon.png      // launcher icon for extra-extra-extra-high-density



## Converting dp and sp to pixels
When you need to set a pixel value for something like `Paint.setTextSize` but still want it be scaled based on the device, you can convert dp and sp values.

    DisplayMetrics metrics = Resources.getSystem().getDisplayMetrics();
    float pixels = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, 12f, metrics);

    DisplayMetrics metrics = Resources.getSystem().getDisplayMetrics();
    float pixels = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 12f, metrics);

Alternatively, you can convert a dimension resource to pixels if you have a context to load the resource from.

    <?xml version="1.0" encoding="utf-8"?>
    <resources>
        <dimen name="size_in_sp">12sp</dimen>
        <dimen name="size_in_dp">12dp</dimen>
    </resources>

    // Get the exact dimension specified by the resource
    float pixels = context.getResources().getDimension(R.dimen.size_in_sp);
    float pixels = context.getResources().getDimension(R.dimen.size_in_dp);

    // Get the dimension specified by the resource for use as a size.
    // The value is rounded down to the nearest integer but is at least 1px.
    int pixels = context.getResources().getDimensionPixelSize(R.dimen.size_in_sp);
    int pixels = context.getResources().getDimensionPixelSize(R.dimen.size_in_dp);

    // Get the dimension specified by the resource for use as an offset.
    // The value is rounded down to the nearest integer and can be 0px.
    int pixels = context.getResources().getDimensionPixelOffset(R.dimen.size_in_sp);
    int pixels = context.getResources().getDimensionPixelOffset(R.dimen.size_in_dp);

## Text size and different android screen sizes
    
Sometimes, it's better to have only three options

     style="@android:style/TextAppearance.Small"
     style="@android:style/TextAppearance.Medium"
     style="@android:style/TextAppearance.Large"

Use small and large to differentiate from normal screen size.

    <TextView
            android:id="@+id/TextViewTopBarTitle"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            style="@android:style/TextAppearance.Small"/>

For normal, you don't have to specify anything.

    <TextView
            android:id="@+id/TextViewTopBarTitle"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"/>

Using this, you can avoid testing and specifying dimensions for different screen sizes.


