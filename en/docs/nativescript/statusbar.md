---
title: "StatusBar"
slug: "statusbar"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Hide/show - android
   This is a statusbar that you see on top of your screen with icons of battry,clock ... .
  [![enter image description here][1]][1]

   
    let frame = require("ui/frame");

   **Hide:**
                                    
    frame.topmost().android.activity.getWindow().
    getDecorView().setSystemUiVisibility(android.view.View.SYSTEM_UI_FLAG_FULLSCREEN);


   **Show:**

    frame.topmost().android.activity.getWindow().
    getDecorView().setSystemUiVisibility(android.view.View.SYSTEM_UI_FLAG_VISIBLE );
 
  [1]: http://i.stack.imgur.com/Ohtul.png

## Make statusBar Transparent  android
open `APP_Resources/values/styles.xml` and add the

``` <item name="android:windowTranslucentStatus">true</item> ```

in the

```    
<style name="AppThemeBase" parent="Theme.AppCompat.Light.NoActionBar"> </style> 
```
section.




