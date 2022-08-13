---
title: "OpenCV initialization in Android"
slug: "opencv-initialization-in-android"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Async Initialization
Using async initialization is a recommended way for application development. It uses the [OpenCV Manager][1] to access OpenCV libraries externally installed in the target system.

Code snippet implementing the async initialization:

    public class MainActivity extends Activity implements CvCameraViewListener2 {
    
        private BaseLoaderCallback mLoaderCallback = new BaseLoaderCallback(this) {
            @Override
            public void onManagerConnected(int status) {
                switch(status) {
                    case LoaderCallbackInterface.SUCCESS:
                        Log.i(TAG,"OpenCV Manager Connected");
                        //from now onwards, you can use OpenCV API
                        Mat m = new Mat(5, 10, CvType.CV_8UC1, new Scalar(0));
                        break;
                    case LoaderCallbackInterface.INIT_FAILED:
                        Log.i(TAG,"Init Failed");
                        break;
                    case LoaderCallbackInterface.INSTALL_CANCELED:
                        Log.i(TAG,"Install Cancelled");
                        break;
                    case LoaderCallbackInterface.INCOMPATIBLE_MANAGER_VERSION:
                        Log.i(TAG,"Incompatible Version");
                        break;
                    case LoaderCallbackInterface.MARKET_ERROR:
                        Log.i(TAG,"Market Error");
                        break;
                    default:
                        Log.i(TAG,"OpenCV Manager Install");
                        super.onManagerConnected(status);
                        break;
                }
            }
        };
    
        @Override
        public void onResume() {
            super.onResume();
            OpenCVLoader.initAsync(OpenCVLoader.OPENCV_VERSION_3_1_0, this, mLoaderCallback);
        }
    
        ...
    }

In this case, our application works with OpenCV Manager in asynchronous fashion. `OnManagerConnected` callback will be called in UI thread, when initialization finishes. 

> Please note, that it is not allowed to use OpenCV calls or load
> OpenCV-dependent native libs before invoking this callback. Load your
> own native libraries that depend on OpenCV after the successful OpenCV
> initialization.

Default `BaseLoaderCallback` implementation treat application context as `Activity` and calls `Activity.finish()` method to exit in case of initialization failure. To override this behaviour you need to override `finish()` method of `BaseLoaderCallback` class and implement your own finalization method.

## OpenCV Manager

OpenCV Manager is an Android service targeted to manage OpenCV library binaries on end users devices. It allows sharing the OpenCV dynamic libraries between applications on the same device. 

The Manager provides the following benefits:

- Less memory usage (around 40MB). All apps use the same binaries from service and do not keep native libs inside themselves.
- Hardware specific optimizations for all supported platforms.
- Trusted OpenCV library source. All packages with OpenCV are published on Google Play market.
- Regular updates and bug fixes.

The only disadvantage is that the user is prompted to download and extra app, so the user experience slightly decreases.

More info: [Android OpenCV Manager][2]

> **Updated 18/10/16:**  
> There is a bug in the OpenCV Manager version distributed on [Play Store][1]
> (updated 21/09/15).  
> It affects only OpenCV 3.1.0 version. When you run some
> OpenCV functions you get a `SIGSEGV` error.
> The version distributed with Android SDK works fine
> (`OpenCV-android-sdk/apk/OpenCV_3.1.0_Manager_3.10_{platform}.apk`).
> It can be downloaded from [OpenCV website][3].  
> More info: [Issue #6247][4].

[1]: https://play.google.com/store/apps/details?id=org.opencv.engine
[2]: http://docs.opencv.org/3.0-beta/platforms/android/service/doc/index.html
[3]: http://opencv.org/downloads.html
[4]: https://github.com/opencv/opencv/issues/6247

## Static Initialization
According to this approach all OpenCV binaries are included into your application package. It is designed mostly for development and debugging purposes. This approach is **deprecated** for the production code, async initialization is recommended.

If your application project doesn’t have a JNI part, just copy the corresponding OpenCV native libs from `OpenCV-3.1.0-android-sdk/sdk/native/libs` to your project directory to folder `app/src/main/jniLibs`.

In case of the application project with a JNI part, instead of manual libraries copying you need to modify your `Android.mk` file: add the following two code lines after the `"include $(CLEAR_VARS)"` and before `"include path_to_OpenCV-3.1.0-android-sdk/sdk/native/jni/OpenCV.mk"`:

    OPENCV_CAMERA_MODULES:=on
    OPENCV_INSTALL_MODULES:=on

The result should look like the following:

    include $(CLEAR_VARS)
    # OpenCV
    OPENCV_CAMERA_MODULES:=on
    OPENCV_INSTALL_MODULES:=on
    include ../../sdk/native/jni/OpenCV.mk

After that the OpenCV libraries will be copied to your application `jniLibs` folder during the JNI build.

The last step of enabling OpenCV in your application is Java initialization code before calling OpenCV API. It can be done, for example, in the static section of the Activity class:

    static {
        if (!OpenCVLoader.initDebug()) {
            // Handle initialization error
        }
    }

If you application includes other OpenCV-dependent native libraries you should load them after OpenCV initialization:

    static {
        if (!OpenCVLoader.initDebug()) {
            // Handle initialization error
        } else {
            System.loadLibrary("my_jni_lib1");
            System.loadLibrary("my_jni_lib2");
        }
    }

> **Note:** [`initDebug()`][1] method is deprecated for production code.
> It is designed for experimental and local development purposes only. 
> If you want to publish your app use approach with async initialization.

[1]: http://docs.opencv.org/3.0-beta/platforms/android/service/doc/JavaHelper.html#boolean-initdebug

