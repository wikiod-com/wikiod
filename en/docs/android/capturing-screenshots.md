---
title: "Capturing Screenshots"
slug: "capturing-screenshots"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Capturing Screenshot via Android Studio
 1. Open Android Monitor Tab
 2. Click on Screen Capture Button
[![Android Studio][1]][1]


  [1]: http://i.stack.imgur.com/Am6Gt.png


## Capturing Screenshot via ADB and saving directly in your PC
If you use Linux (or Windows with Cygwin), you can run:

    adb shell screencap -p | sed 's/\r$//' > screenshot.png

## Taking a screenshot of a particular view
If you want to take a screenshot of a particular View `v`, then you can use the following code:

    Bitmap viewBitmap = Bitmap.createBitmap(v.getWidth(), v.getHeight(), Bitmap.Config.RGB_565);
    Canvas viewCanvas = new Canvas(viewBitmap);
    Drawable backgroundDrawable = v.getBackground();
    
    if(backgroundDrawable != null){
        // Draw the background onto the canvas.
        backgroundDrawable.draw(viewCanvas);
    }
    else{
        viewCanvas.drawColor(Color.GREEN);
        // Draw the view onto the canvas.
        v.draw(viewCanvas) 
    }
    
    // Write the bitmap generated above into a file.
    String fileStamp = new SimpleDateFormat("yyyyMMdd_HHmmss").format(new Date());
    OutputStream outputStream = null;
    try{
        imgFile = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), fileStamp + ".png");
        outputStream = new FileOutputStream(imgFile);
        viewBitmap.compress(Bitmap.CompressFormat.PNG, 40, outputStream);
        outputStream.close();
    }
    catch(Exception e){
        e.printStackTrace();
    }

## Capturing Screenshot via Android Device Monitor
 1. Open Android Device Monitor (<i> ie C:\<ANDROID_SDK_LOCATION>\tools\monitor.bat</i>)
 2. Select your device
 3. Click on Screen Capture Button

[![Android Device Monitor][1]][1]


  [1]: http://i.stack.imgur.com/TDMzE.png


## Capturing Screenshot via ADB
Example below saves a screenshot on Devices's Internal Storage.

    adb shell screencap /sdcard/screen.png

