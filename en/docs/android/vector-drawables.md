---
title: "Vector Drawables"
slug: "vector-drawables"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

As the name implies, vector drawables are based on vector graphics. Vector graphics are a way of describing graphical elements using geometric shapes. This lets you create a drawable based on an XML vector graphic.

Now there is no need to design different size image for mdpi, hdpi, xhdpi and etc. With Vector Drawable you need to create image only once as an xml file and you can scale it for all dpi and for different devices. This also not save space but also simplifies maintenance.



## Parameters
|Parameter|Details|
|-----|------|
|`<vector>`|Used to define a vector drawable|
|`<group>`|Defines a group of paths or subgroups, plus transformation information. The transformations are defined in the same coordinates as the viewport. And the transformations are applied in the order of scale, rotate then translate.|
|`<path>`|Defines paths to be drawn.|
|`<clip-path>`|Defines path to be the current clip. Note that the clip path only apply to the current group and its children.|


Update **build.gradle** file.

    dependencies {
        ...
       compile 'com.android.support:appcompat-v7:23.2.1'
    }

If you are using **v2.0 or above** of the **Gradle plugin**, then add following code.

    // Gradle Plugin 2.0+  
     android {  
       defaultConfig {  
         vectorDrawables.useSupportLibrary = true  
        }  
     }

If you are using **v1.5 or below** of the **Gradle plugin**, then add following code.

    // Gradle Plugin 1.5  
     android {  
       defaultConfig {  
         generatedDensities = []  
      }  
    
      // This is handled for you by the 2.0+ Gradle Plugin  
      aaptOptions {  
        additionalParameters "--no-version-vectors"  
      }  
     }

Read [Android Support Library 23.2 Release Notes](https://android-developers.googleblog.com/2016/02/android-support-library-232.html) for more info.

**NOTE :** Even with *AppCompat*, Vector Drawables wont work outside of your app in older android versions. For instance, you cannot pass vector drawables as Notification icons as they are handled by the system and not the app. See [this answer](http://stackoverflow.com/a/37334176/3894781) for a workaround.

## VectorDrawable Usage Example
Here’s an example vector asset which we’re actually using in AppCompat:

**res/drawable/ic_search.xml**

    <vector xmlns:android="..."
            android:width="24dp"
            android:height="24dp"
            android:viewportWidth="24.0"
            android:viewportHeight="24.0"
            android:tint="?attr/colorControlNormal">
    
        <path
            android:pathData="..."
            android:fillColor="@android:color/white"/>
    
    </vector>

Using this drawable, an example `ImageView` declaration would be:

    <ImageView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        app:srcCompat="@drawable/ic_search"/>

You can also set it at run-time:

    ImageView iv = (ImageView) findViewById(...);
    iv.setImageResource(R.drawable.ic_search);

The same attribute and calls work for `ImageButton` too.

## VectorDrawable xml example

Here is a simple `VectorDrawable` in this **vectordrawable.xml** file.

     <vector xmlns:android="http://schemas.android.com/apk/res/android"
         android:height="64dp"
         android:width="64dp"
         android:viewportHeight="600"
         android:viewportWidth="600" >
         <group
             android:name="rotationGroup"
             android:pivotX="300.0"
             android:pivotY="300.0"
             android:rotation="45.0" >
             <path
                 android:name="v"
                 android:fillColor="#000000"
                 android:pathData="M300,70 l 0,-70 70,70 0,0 -70,70z" />
         </group>
     </vector>

## Importing SVG file as VectorDrawable
You can import an _SVG_ file as a `VectorDrawable` in Android Studio, follow these steps :

"Right-click" on the `res` folder and select **new** > **Vector Asset**.

[![enter image description here][1]][1]

Select the **Local File** option and browse to your .svg file. Change the options to your liking and hit next. Done.

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/5eQyy.png
  [2]: https://i.stack.imgur.com/zCsJ9.png

