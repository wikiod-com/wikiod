---
title: "Getting started with android-edittext"
slug: "getting-started-with-android-edittext"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Specifying Text Hints
It is possible to specify a text hint when using EditTexts. Text hints are useful for conveying to the user what they should type in the EditText.

In XML:

    <EditText
        android:layout_width="match_parent"
        android:layout_height="wrap_content"
        android:hint="username" />
The content of the hint (in our example, 'username') can be anything you like.

## Styling In EditText
   
   Changing the edit text's appearance when it's selected, pressed and not selected can be customised easily by adding creating a new style for your edit text like so

    <style name="EditTextTheme" parent="Theme.AppCompat.Light.DarkActionBar">
        <item name="colorControlNormal">@color/colorPrimary</item>
        <item name="colorControlActivated">@color/colorPrimaryDark</item>
        <item name="colorControlHighlight">@color/accent</item>
    </style>

And then add this style to your EditText like

    <EditText
        android:width="wrap_content"
        android:height="wrap_content"
        style="@style/EditTextTheme" />

 

