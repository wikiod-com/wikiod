---
title: "Define step value (increment) for custom RangeSeekBar"
slug: "define-step-value-increment-for-custom-rangeseekbar"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

A customization of the Android RangeSeekBar proposed by Alex Florescu at https://github.com/anothem/android-range-seek-bar

It allows to define a step value (increment), when moving the seek bar

1- Add the increment attribute in attrs.xml

    <attr name="increment" format="integer|float"/>

2- Define a default value in RangeSeekBar.java and create the attribute also

    private static final int DEFAULT_INCREMENT = 1;
    private int increment;

3- Init the increment value in private void init(Context context, AttributeSet attrs)

    if (attrs == null) 
        increment = DEFAULT_INCREMENT;
    else 
        increment = a.getInt(R.styleable.RangeSeekBar_increment, DEFAULT_INCREMENT);

4- Define the increment value in protected synchronized void onDraw(@NonNull Canvas canvas)

You'll have to replace the minText and maxText value. So instead of :

 - minText = valueToString(getSelectedMinValue());
 - maxText = valueToString(getSelectedMaxValue());

You'll have :
            int x;
            
            x = (int) ((getSelectedMinValue().intValue()+increment)/increment);
            x = x*increment;
            if (x<absoluteMaxValue.intValue()) 
                minText = ""+x;
            else
                minText=""+(absoluteMaxValue.intValue()-increment);
            
            
            x = (int) ((getSelectedMaxValue().intValue()+increment)/increment);
            x = x*increment;
            maxText = ""+x;

5 - Now you just have to use it. Hope it helps

## Define a step value of 7
    <RangeSeekBar
            android:id="@+id/barPrice"
            android:layout_width="fill_parent"
            android:layout_height="wrap_content"
            app:barHeight="0.2dp"
            app:barHeight2="4dp"
            app:increment="7"
            app:showLabels="false" />

