---
title: "Getting started with android-layout"
slug: "getting-started-with-android-layout"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Linear Layout
> LinearLayout is a view group that aligns all children in a single
> direction, vertically or horizontally.



     <?xml version="1.0" encoding="utf-8"?>
        <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
            android:layout_width="match_parent"
            android:layout_height="match_parent"
            android:paddingLeft="16dp"
            android:paddingRight="16dp"
            android:orientation="vertical" >
            <EditText
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="@string/to" />
            <EditText
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="@string/subject" />
            <EditText
                android:layout_width="match_parent"
                android:layout_height="0dp"
                android:layout_weight="1"
                android:gravity="top"
                android:hint="@string/message" />
            <Button
                android:layout_width="100dp"
                android:layout_height="wrap_content"
                android:layout_gravity="right"
                android:text="@string/send" />
        </LinearLayout>

One more important attribute in linear layout is **Layout Weight**

LinearLayout also supports assigning a weight to individual children with the android:layout_weight attribute. This attribute assigns an "importance" value to a view in terms of how much space it should occupy on the screen. 

**LinearLayout Attributes**

| Attribute | Description |
| ------ | ------ |
| android:orientation   | This specifies the direction of arrangement and you will use "horizontal" for a row, "vertical" for a column. The default is horizontal.   |
| android:weightSum | Sum up of child weight |
| android:gravity | This specifies how an object should position its content, on both the X and Y axes. Possible values are top, bottom, left, right, center, center_vertical, center_horizontal etc. |
| android:divider | This is drawable to use as a vertical divider between buttons. You use a color value, in the form of "#rgb", "#argb", "#rrggbb", or "#aarrggbb".|


## Installation or Setup
Detailed instructions on getting android-layout set up or installed.

