---
title: "Universal Image Loader"
slug: "universal-image-loader"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Acceptable URI examples:

    "http://www.example.com/image.png" // from Web
    "file:///mnt/sdcard/image.png" // from SD card
    "file:///mnt/sdcard/video.mp4" // from SD card (video thumbnail)
    "content://media/external/images/media/13" // from content provider
    "content://media/external/video/media/13" // from content provider (video thumbnail)
    "assets://image.png" // from assets
    "drawable://" + R.drawable.img // from drawables (non-9patch images)

## Basic usage
1. Load an image, decode it into a bitmap, and display the bitmap in an `ImageView` (or any other view which implements the `ImageAware` interface):

       ImageLoader.getInstance().displayImage(imageUri, imageView);

2. Load an image, decode it into a bitmap, and return the bitmap to a callback:

       ImageLoader.getInstance().loadImage(imageUri, new SimpleImageLoadingListener() {
           @Override
           public void onLoadingComplete(String imageUri, View view, Bitmap loadedImage) {
               // Do whatever you want with the bitmap.
           }
       });

3. Load an image, decode it into a bitmap and return the bitmap synchronously:

       Bitmap bmp = ImageLoader.getInstance().loadImageSync(imageUri);

## Initialize Universal Image Loader
1. Add the following dependency to the _build.gradle_ file:

       compile 'com.nostra13.universalimageloader:universal-image-loader:1.9.5'

2. Add the following permissions to the _AndroidManifest.xml_ file:

       <uses-permission android:name="android.permission.INTERNET" />
       <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />

3. Initialize the Universal Image Loader. This must be done before the first usage: 

       ImageLoaderConfiguration config = new ImageLoaderConfiguration.Builder(this)
           // ...
           .build();
       ImageLoader.getInstance().init(config);

   The full configuration options can be found [here][1].

  [1]: https://github.com/nostra13/Android-Universal-Image-Loader/wiki/Configuration

