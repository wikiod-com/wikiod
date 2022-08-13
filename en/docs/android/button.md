---
title: "Button"
slug: "button"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## Syntax
 - <Button ... /> 
 - android:onClick="methodname"
 - button.setOnClickListener(new OnClickListener(){...});
 - public class classname implements View.OnLongClickListener 

## Using the same click event for one or more Views in the XML


## inline onClickListener
Say we have a button (we can create it programmatically, or bind it from a view using findViewbyId(), etc...)   

    Button btnOK = (...) 

Now, create an anonymous class and set it inline.

    btnOk.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            // Do stuff here...
        }
    });

## Defining external Listener


## Customizing Button style
There are many possible ways of customizing the look of a Button. This example presents several options:

--------------------
**Option 0: Use ThemeOverlay (currently the easiest/quickest way)**

Create a new style in your styles file:

**styles.xml**

    <resources>
        <style name=“mybutton” parent=”ThemeOverlay.AppCompat.Ligth”>
            <!-- customize colorButtonNormal for the disable color -->
            <item name="colorButtonNormal">@color/colorbuttonnormal</item>
            <!-- customize colorAccent for the enabled color -->
            <item name="colorButtonNormal">@color/coloraccent</item>
        </style>
    </resources>
Then in the layout where you place your button (e.g. MainActivity):

**activity_main.xml**

    <?xml version="1.0" encoding="utf-8"?>
    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:layout_gravity="center_horizontal"
        android:gravity="center_horizontal"
        android:orientation="vertical"
        android:paddingBottom="@dimen/activity_vertical_margin"
        android:paddingLeft="@dimen/activity_horizontal_margin"
        android:paddingRight="@dimen/activity_horizontal_margin"
        android:paddingTop="@dimen/activity_vertical_margin"
        tools:context=".MainActivity">
    
                <Button
                    android:id="@+id/mybutton"
                    android:layout_width="wrap_content"
                    android:layout_height="wrap_content"
                    android:text="Hello"
                    android:theme="@style/mybutton"
                    style="@style/Widget.AppCompat.Button.Colored"/>
    
    </LinearLayout>


--------------------
**Option 1: Create your own button style**

In values/styles.xml, create a new style for your button:

**styles.xml**

    <resources>
            <style name="mybuttonstyle" parent="@android:style/Widget.Button">
                <item name="android:gravity">center_vertical|center_horizontal</item>
                <item name="android:textColor">#FFFFFFFF</item>
                <item name="android:shadowColor">#FF000000</item>
                <item name="android:shadowDx">0</item>
                <item name="android:shadowDy">-1</item>
                <item name="android:shadowRadius">0.2</item>
                <item name="android:textSize">16dip</item>
                <item name="android:textStyle">bold</item>
                <item name="android:background">@drawable/button</item>
            </style>
        </resources>

Then in the layout where you place your button (e.g. in MainActivity):

**activity_main.xml**

    <?xml version="1.0" encoding="utf-8"?>
    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:layout_gravity="center_horizontal"
        android:gravity="center_horizontal"
        android:orientation="vertical"
        android:paddingBottom="@dimen/activity_vertical_margin"
        android:paddingLeft="@dimen/activity_horizontal_margin"
        android:paddingRight="@dimen/activity_horizontal_margin"
        android:paddingTop="@dimen/activity_vertical_margin"
        tools:context=".MainActivity">
    
                <Button
                    android:id="@+id/mybutton"
                    android:layout_width="wrap_content"
                    android:layout_height="wrap_content"
                    android:text="Hello"
                    android:theme="@style/mybuttonstyle"/>
    
    </LinearLayout>

--------------------
**Option 2: Assign a drawable for each of your button states**

Create an xml file into drawable folder called 'mybuttondrawable.xml' to define the drawable resource of each of your button states:

**drawable/mybutton.xml**

<?xml version="1.0" encoding="utf-8"?>

    <selector xmlns:android="http://schemas.android.com/apk/res/android">
        <item
            android:state_enabled="false"
            android:drawable="@drawable/mybutton_disabled" />
        <item
            android:state_pressed="true"
            android:state_enabled="true"
            android:drawable="@drawable/mybutton_pressed" />
        <item
            android:state_focused="true"
            android:state_enabled="true"
            android:drawable="@drawable/mybutton_focused" />
        <item
            android:state_enabled="true"
            android:drawable="@drawable/mybutton_enabled" />
    </selector>

Each of those drawables may be images (e.g. mybutton_disabled.png) or xml files defined by you and stored in the drawables folder. For instance:

**drawable/mybutton_disabled.xml**

    <?xml version="1.0" encoding="utf-8"?>
    
        <shape xmlns:android="http://schemas.android.com/apk/res/android" android:shape="rectangle">
            <gradient
                android:startColor="#F2F2F2"
                android:centerColor="#A4A4A4"
                android:endColor="#F2F2F2"
                android:angle="90"/>
            <padding android:left="7dp"
                android:top="7dp"
                android:right="7dp"
                android:bottom="7dp" />
            <stroke
                android:width="2dip"
                android:color="#FFFFFF" />
            <corners android:radius= "8dp" />
        </shape>

Then in the layout where you place your button (e.g. MainActivity):

**activity_main.xml**

    <?xml version="1.0" encoding="utf-8"?>
    <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:tools="http://schemas.android.com/tools"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:layout_gravity="center_horizontal"
        android:gravity="center_horizontal"
        android:orientation="vertical"
        android:paddingBottom="@dimen/activity_vertical_margin"
        android:paddingLeft="@dimen/activity_horizontal_margin"
        android:paddingRight="@dimen/activity_horizontal_margin"
        android:paddingTop="@dimen/activity_vertical_margin"
        tools:context=".MainActivity">
    
                <Button
                    android:id="@+id/mybutton"
                    android:layout_width="wrap_content"
                    android:layout_height="wrap_content"
                    android:text="Hello"
                    android:background="@drawable/mybuttondrawable"/>
    
    </LinearLayout>


--------------------
**Option 3: Add your button style to your App theme**

You can override the default android button style in the definition of your app theme (in values/styles.xml).

**styles.xml**

    <resources>
         <style name="AppTheme" parent="android:Theme">
                <item name="colorPrimary">@color/colorPrimary</item>
                <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
                <item name="colorAccent">@color/colorAccent</item>
                  <item name="android:button">@style/mybutton</item>
         </style>
        
         <style name="mybutton" parent="android:style/Widget.Button">
                 <item name="android:gravity">center_vertical|center_horizontal</item>
                    <item name="android:textColor">#FFFFFFFF</item>
                    <item name="android:shadowColor">#FF000000</item>
                    <item name="android:shadowDx">0</item>
                    <item name="android:shadowDy">-1</item>
                    <item name="android:shadowRadius">0.2</item>
                    <item name="android:textSize">16dip</item>
                    <item name="android:textStyle">bold</item>
                    <item name="android:background">@drawable/anydrawable</item>
         </style>
    </resources>

--------------------
**Option 4: Overlay a color on the default button style programatically**

Just find you button in your activity and apply a color filter:

    Button mybutton = (Button) findViewById(R.id.mybutton);
    mybutton.getBackground().setColorFilter(anycolor, PorterDuff.Mode.MULTIPLY)

You can check different blending modes [here][1] and nice examples [here][2].


  [1]: https://developer.android.com/reference/android/graphics/PorterDuff.Mode.html
  [2]: http://ssp.impulsetrain.com/porterduff.html


## Using the layout to define a click action


## Listening to the long click events


## Custom Click Listener to prevent multiple fast clicks


