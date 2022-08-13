---
title: "Theme, Style, Attribute"
slug: "theme-style-attribute"
draft: false
images: []
weight: 9883
type: docs
toc: true
---

## Define primary, primary dark, and accent colors
You can customize your [theme’s color palette][1].

Using **framework** APIs

<!-- if version [gte 5.0] -->

    <style name="AppTheme" parent="Theme.Material">
        <item name="android:colorPrimary">@color/primary</item>
        <item name="android:colorPrimaryDark">@color/primary_dark</item>
        <item name="android:colorAccent">@color/accent</item>
    </style>

<!-- end version if -->

Using the **Appcompat support library** (and `AppCompatActivity`)

<!-- if version [gte 2.1.x] -->

    <style name="AppTheme" parent="Theme.AppCompat">
        <item name="colorPrimary">@color/primary</item>
        <item name="colorPrimaryDark">@color/primary_dark</item>
        <item name="colorAccent">@color/accent</item>
    </style>

<!-- end version if -->


  [1]: https://developer.android.com/training/material/theme.html?#ColorPalette

## Navigation Bar Color (API 21+)
<!-- if version [gte 5.0] -->

This attribute is used to change the navigation bar (one, that contain Back, Home Recent button). Usually it is black, however it's color can be changed.
```
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:navigationBarColor">@color/my_color</item>
</style>
```

<!-- end version if -->



## Multiple Themes in one App


## Use Custom Theme Per Activity
In themes.xml:

    <style name="MyActivityTheme" parent="Theme.AppCompat">
        <!-- Theme attributes here -->
    </style>

In AndroidManifest.xml:

    <application
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:theme="@style/Theme.AppCompat">
        
        <activity
            android:name=".MyActivity"
            android:theme="@style/MyActivityTheme" />
    
    </application>

## Light Status Bar (API 23+)
This attribute can change the background of the Status Bar icons (at the top of the screen) to white.
```
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:windowLightStatusBar">true</item>
</style>
```

## Use Custom Theme Globally
In themes.xml:

    <style name="AppTheme" parent="Theme.AppCompat">
        <!-- Theme attributes here -->
    </style>

In AndroidManifest.xml:

    <application
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:theme="@style/AppTheme">
        
        <!-- Activity declarations here -->
    
    </application>

## Overscroll Color (API 21+)
```
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:colorEdgeEffect">@color/my_color</item>
</style>
```


## Ripple Color (API 21+)
<!-- if version [gte 5.0] -->
The [ripple][1] animation is shown when user presses clickable views.

You can use the same ripple color used by your app assigning the  `?android:colorControlHighlight` in your views. You can customize this color by changing the `android:colorControlHighlight` attribute in your theme:

This effect color can be changed:

    <style name="AppTheme" parent="Theme.AppCompat">
        <item name="android:colorControlHighlight">@color/my_color</item>
    </style>

Or, if you are using a Material Theme:

    <style name="AppTheme" parent="android:Theme.Material.Light">
        <item name="android:colorControlHighlight">@color/your_custom_color</item>
    </style>

<!-- end version if -->


  [1]: https://www.wikiod.com/android/material-design#RippleDrawable

## Translucent Navigation and Status Bars (API 19+)
The navigation bar (at the bottom of the screen) can be transparent. Here is the way to achieve it.
```
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:windowTranslucentNavigation">true</item>
</style>
```

The Status Bar (top of the screen) can be made transparent, by applying this attribute to the style:
```
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:windowTranslucentStatus">true</item>
</style>
```

## Theme inheritance
When defining themes, one usually uses the theme provided by the system, and then changes modifies the look to fit his own application. For example, this is how the `Theme.AppCompat` theme is inherited:

    <style name="AppTheme" parent="Theme.AppCompat">
        <item name="colorPrimary">@color/colorPrimary</item>
        <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
        <item name="colorAccent">@color/colorAccent</item>
    </style>

This theme now has all the properties of the standard `Theme.AppCompat` theme, except the ones we explicitly changed.

There is also a shortcut when inheriting, usually used when one inherits from his own theme:

    <style name="AppTheme.Red">
        <item name="colorAccent">@color/red</item>
    </style>

Since it already has `AppTheme.` in the start of it's name, it automatically inherits it, without needing to define the `parent` theme. This is useful when you need to create specific styles for a part (for example, a single Activity) of your app.



