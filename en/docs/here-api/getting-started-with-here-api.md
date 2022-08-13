---
title: "Getting started with here-api"
slug: "getting-started-with-here-api"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Create a New Android Studio Project
 1. From the Welcome to Android Studio dialogue box, select New Project... to open the Create New Project dialog. 
 2. In the New Android Application dialog, under Application name, specify an appropriate application name. The remainder of this tutorial uses BasicMapSolution as the application name. 
 3. Under Company Domain, specify an appropriate domain. 
 4. Edit the package name by clicking the Edit link. The remainder of this tutorial uses com.here.android.tutorial as the package name.

    **Important**: You must use the same package name as you have registered on developer.here.com. Failure to do so leads to a blank map to appear in your application.

    You can also edit this package name later in your AndroidManifest.xml: 
   

     <manifest xmlns:android="http://schemas.android.com/apk/res/android"
          package="com.your.package.name.here"
          android:versionCode="1"
          android:versionName="1.0" >

 5.  Under Project Location, specify an appropriate project location in the file system. 

 6. Click Next.

 7.  Select the form factors supported by your application. For the purpose of this tutorial, check Phone and Tablet. 

 8. Under Minimum SDK, select the lowest version of the Android SDK you wish to support. For this sample application, use Android 4.0.3 "Ice Cream Sandwich".

 9. Click Next.

 10. You may be prompted to agree to a License Agreement. Click Accept, and then Next to install SDK components. After the installation is complete, click Next again. 

 11. In the "Add an activity to Mobile" dialog box, select Empty Activity and click Next. 

 12. In the "Customize Activity" dialog box, specify an appropriate activity name in Activity Name. This tutorial uses the name BasicMapActivity. 

 13. Under Layout Name, specify an appropriate layout name. (This tutorial uses activity_main.) 

 14. Click Finish. 





## Initializing the Map Fragment
When you have defined the basic layout of the application and acquired necessary permissions, the final step is to initialize the instance of the MapFragment class, thus creating and associating a Map with the MapFragment declared in the activity_main.xml file.

    public class BasicMapActivity extends Activity {
    
      // map embedded in the map fragment
      private Map map = null;
    
      // map fragment embedded in this activity
      private MapFragment mapFragment = null;
    
      @Override
      public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        initialize();
      }
    
      private void initialize() {
        setContentView(R.layout.activity_main);
    
        mapFragment = (MapFragment)getFragmentManager().findFragmentById(R.id.mapfragment);
        mapFragment.init(new OnEngineInitListener() {
          @Override
          public void onEngineInitializationCompleted(OnEngineInitListener.Error error)
          {
            if (error == OnEngineInitListener.Error.NONE) {
              map = mapFragment.getMap();  
              map.setCenter(new GeoCoordinate(49.196261, -123.004773, 0.0),
                      Map.Animation.NONE);  
              map.setZoomLevel(
                (map.getMaxZoomLevel() + map.getMinZoomLevel()) / 2);
            } else {
              System.out.println("ERROR: Cannot initialize Map Fragment");
            }
          }
        });
      }
    }

## Acquire HERE SDK Credentials
Typically, before developing a new HERE SDK application, you need to acquire a set of credentials by registering your application on http://developer.here.com. Each application requires a unique set of credentials. When you register your app, the registered bundle identifier must match the package name in your project. 

Each application, commercial or evaluation requires an appid and app code pair to identify the company. Additionally, for the HERE Premium SDK a "license key" is used to provide access to advanced turn by turn and offline features.

For premium SDK, please see:
[https://developer.here.com/mobile-sdks/documentation/android-hybrid-plus/topics/credentials.html][1]

For starter SDK, please see:
[https://developer.here.com/mobile-sdks/documentation/android/topics/credentials.html][1]


  [1]: https://developer.here.com/mobile-sdks/documentation/android/topics/credentials.html

## Offical Github examples
HERE Android Premium SDK samples
Now available on Github! [https://github.com/heremaps/here-android-sdk-examples][1]

HERE iOS Premium SDK samples
Now available on Github! [https://github.com/heremaps/here-ios-sdk-examples][1]

Please refer to the README.md on how to get started. Note the samples require a permission key to operate.

