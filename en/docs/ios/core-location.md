---
title: "Core Location"
slug: "core-location"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

## Syntax
 1. desiredAccuracy
 2. distanceFilter
 3. requestLocation()
 4. startUpdatingLocation()
 5. allowDeferredLocationUpdates(untilTraveled:timeout:)
 6. startMonitoringSignificantLocationChanges()
 7. allowDeferredLocationUpdates(untilTraveled:timeout:) 
 8. authorizedAlways
 9. authorizedWhenInUse
 10. locationManager(_:didChangeAuthorization:)

# [Simulate a Location at Runtime][1] #
1. Run the app from Xcode.
2. In the debug bar, click the "Simulate location" button.
3. Choose a location from the menu.

[![Simulate location - debug][2]][2]


  [1]: https://developer.apple.com/library/ios/recipes/xcode_help-debugger/articles/simulating_locations.html
  [2]: http://i.stack.imgur.com/e02nU.png

## Request Permission to Use Location Services
Check the app's authorization status with:
    
    //Swift
    let status: CLAuthorizationStatus = CLLocationManager.authorizationStatus()

    //Objective-C
    CLAuthorizationStatus status = [CLLocationManager authorizationStatus];

Test the status against the follow constants:
    
    //Swift
    switch status {
    case .NotDetermined:
        // Do stuff
    case .AuthorizedAlways:
        // Do stuff
    case .AuthorizedWhenInUse:
        // Do stuff
    case .Restricted:
        // Do stuff
    case .Denied:
        // Do stuff
    }

    //Objective-C
    switch (status) {
        case kCLAuthorizationStatusNotDetermined:
            
            //The user hasn't yet chosen whether your app can use location services or not.
            
            break;
            
        case kCLAuthorizationStatusAuthorizedAlways:
            
            //The user has let your app use location services all the time, even if the app is in the background.
            
            break;
            
        case kCLAuthorizationStatusAuthorizedWhenInUse:
            
            //The user has let your app use location services only when the app is in the foreground.
            
            break;
            
        case kCLAuthorizationStatusRestricted:
            
            //The user can't choose whether or not your app can use location services or not, this could be due to parental controls for example.
            
            break;
            
        case kCLAuthorizationStatusDenied:
            
            //The user has chosen to not let your app use location services.
            
            break;
            
        default:
            break;
    }

---
# Getting Location Service Permission While App is in Use
[![Location When In Use Usage Dialog][1]][1]

Simplest method is to initialize the location manager as a property of your root view controller and place the permission request in its `viewDidLoad`.

This brings up the alert controller that asks for permission:

    //Swift
    let locationManager = CLLocationManager()
    locationManager.requestWhenInUseAuthorization()

    //Objective-C
    CLLocationManager *locationManager = [[CLLocationManager alloc] init];
    [locationManager requestWhenInUseAuthorization];

Add the **NSLocationWhenInUseUsageDescription** key to your *Info.plist*. The value will be used in the alert controller's `message` label.

[![enter image description here][2]][2]

---
# Getting Location Service Permission Always
[![enter image description here][3]][3]

To ask for permission to use location services even when the app is not active, use the following call instead:

    //Swift
    locationManager.requestAlwaysAuthorization()

    //Objective-C
    [locationManager requestAlwaysAuthorization];

Then add the **NSLocationAlwaysUsageDescription** key to your *Info.plist*. Again, the value will be used in the alert controller's `message` label.

[![enter image description here][4]][4]

  [1]: http://i.stack.imgur.com/wT57a.png
  [2]: http://i.stack.imgur.com/evvkE.png
  [3]: http://i.stack.imgur.com/T8wjR.png
  [4]: http://i.stack.imgur.com/HRtxQ.png

## Link CoreLocation Framework
[![Linking CoreLocation Framework][1]][1]

Import the CoreLocation module in your classes that use CoreLocation functionality.

    //Swift
    import CoreLocation

    //Objective-C
    #import <CoreLocation/CoreLocation.h>

  [1]: http://i.stack.imgur.com/bcsu4.png

## Add own custom location using GPX file
 To check for location services we need real device but for testing purpose we can also use simulator and add our own location by following below steps:

- add new GPX file into your project.
- in GPX file add waypoints like 
  

    <?xml version="1.0"?>
    <gpx version="1.1" creator="Xcode"> 
    <!--
            Provide one or more waypoints containing a latitude/longitude pair. If you provide one
            waypoint, Xcode will simulate that specific location. If you provide multiple waypoints,
            Xcode will simulate a route visitng each waypoint.
     -->
    <wpt lat="52.599878" lon="4.702029">
         <name>location name (eg. Florida)</name>
    </wpt>
    
</gpx>

    

- then go to product-->Scheme-->Edit Scheme and into RUN set default location as your GPX file name.


## Location Services in the Background

To use standard location services while the application is in the background you need first turn on `Background Modes` in the Capabilities tab of the Target settings, and select `Location updates`.

[![Background Modes][1]][1]

  [1]: http://cdn2.raywenderlich.com/wp-content/uploads/2014/12/background_modes.png
Or, add it directly to the Info.plist. 

    <key>NSLocationAlwaysUsageDescription</key>
    <string>I want to get your location Information in background</string>
    
    <key>UIBackgroundModes</key>
    <array>
        <string>location</string>
    </array>


Then you need to setup the CLLocationManager

**Objective C**

    //The Location Manager must have a strong reference to it.
    _locationManager = [[CLLocationManager alloc] init];
    _locationManager.delegate = self;
   
    //Request Always authorization (iOS8+)
    if ([_locationManager respondsToSelector:@selector(requestAlwaysAuthorization)]) {
        [_locationManager requestAlwaysAuthorization];
    }

    //Allow location updates in the background (iOS9+)
    if ([_locationManager respondsToSelector:@selector(allowsBackgroundLocationUpdates)]) {
        _locationManager.allowsBackgroundLocationUpdates = YES;
    }

    [_locationManager startUpdatingLocation];

**Swift**

    self.locationManager.delegate = self
    
    if #available (iOS 8.0,*) {
        self.locationManager.requestAlwaysAuthorization()
    }

    if #available (iOS 9.0,*) {
        self.locationManager.allowsBackgroundLocationUpdates = true
    }
    
    self.locationManager.startUpdatingLocation()


