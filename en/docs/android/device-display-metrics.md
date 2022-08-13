---
title: "Device Display Metrics"
slug: "device-display-metrics"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Get the screens pixel dimensions
To retreive the screens width and height in pixels, we can make use of the [WindowManagers][1] display metrics. 

    // Get display metrics
    DisplayMetrics metrics = new DisplayMetrics();
    context.getWindowManager().getDefaultDisplay().getMetrics(metrics);

These [DisplayMetrics ][2] hold a series of information about the devices screen, like its density or size:

    // Get width and height in pixel
    Integer heightPixels = metrics.heightPixels;
    Integer widthPixels = metrics.widthPixels;


  [1]: https://developer.android.com/reference/android/view/WindowManager.html
  [2]: https://developer.android.com/reference/android/util/DisplayMetrics.html

## Get screen density
To get the screens density, we also can make use of the [Windowmanagers][1] [DisplayMetrics][2]. This is a quick example:

    // Get density in dpi
    DisplayMetrics metrics = new DisplayMetrics();
    context.getWindowManager().getDefaultDisplay().getMetrics(metrics);
    int densityInDpi =  metrics.densityDpi;

  [1]: https://developer.android.com/reference/android/view/WindowManager.html
  [2]: https://developer.android.com/reference/android/util/DisplayMetrics.html

## Formula px to dp, dp to px conversation 

**DP to Pixel:**

    private int dpToPx(int dp)
    {
        return (int) (dp * Resources.getSystem().getDisplayMetrics().density);
    }
    

**Pixel to DP:**  

    private int pxToDp(int px)
    {
        return (int) (px / Resources.getSystem().getDisplayMetrics().density);
    }


