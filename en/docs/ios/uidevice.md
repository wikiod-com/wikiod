---
title: "UIDevice"
slug: "uidevice"
draft: false
images: []
weight: 9789
type: docs
toc: true
---

## Parameters
| Property | Description |
| ------ | ------ |
|name | The name identifying the device.|
|systemName: String | The name of the operating system running on the device represented by the receiver.|
|model: String| The model of the device.|
|systemVersion: String | The current version of the operating system..|




    

> The UIDevice class provides a Singleton instance representing the
> current device. From this instance you can obtain information about
> the device such as assigned name, device model, and operating-system
> name and version.

## Get iOS device model name
**Swift 2**    
    
   
    import UIKit
    
    extension UIDevice {
    
        var modelName: String {
            var systemInfo = utsname()
            uname(&systemInfo)
            let machineMirror = Mirror(reflecting: systemInfo.machine)
            let identifier = machineMirror.children.reduce("") { identifier, element in
                guard let value = element.value as? Int8 where value != 0 else { return identifier }
                return identifier + String(UnicodeScalar(UInt8(value)))
            }
        
            switch identifier {
            case "iPod5,1":                                 return "iPod Touch 5"
            case "iPod7,1":                                 return "iPod Touch 6"
            case "iPhone3,1", "iPhone3,2", "iPhone3,3":     return "iPhone 4"
            case "iPhone4,1":                               return "iPhone 4s"
            case "iPhone5,1", "iPhone5,2":                  return "iPhone 5"
            case "iPhone5,3", "iPhone5,4":                  return "iPhone 5c"
            case "iPhone6,1", "iPhone6,2":                  return "iPhone 5s"
            case "iPhone7,2":                               return "iPhone 6"
            case "iPhone7,1":                               return "iPhone 6 Plus"
            case "iPhone8,1":                               return "iPhone 6s"
            case "iPhone8,2":                               return "iPhone 6s Plus"
            case "iPhone9,1", "iPhone9,3":                  return "iPhone 7"
            case "iPhone9,2", "iPhone9,4":                  return "iPhone 7 Plus"
            case "iPhone8,4":                               return "iPhone SE"
            case "iPad2,1", "iPad2,2", "iPad2,3", "iPad2,4":return "iPad 2"
            case "iPad3,1", "iPad3,2", "iPad3,3":           return "iPad 3"
            case "iPad3,4", "iPad3,5", "iPad3,6":           return "iPad 4"
            case "iPad4,1", "iPad4,2", "iPad4,3":           return "iPad Air"
            case "iPad5,3", "iPad5,4":                      return "iPad Air 2"
            case "iPad2,5", "iPad2,6", "iPad2,7":           return "iPad Mini"
            case "iPad4,4", "iPad4,5", "iPad4,6":           return "iPad Mini 2"
            case "iPad4,7", "iPad4,8", "iPad4,9":           return "iPad Mini 3"
            case "iPad5,1", "iPad5,2":                      return "iPad Mini 4"
            case "iPad6,3", "iPad6,4", "iPad6,7", "iPad6,8":return "iPad Pro"
            case "AppleTV5,3":                              return "Apple TV"
            case "i386", "x86_64":                          return "Simulator"
            default:                                        return identifier
            }
        }
    }

    if UIDevice.currentDevice().modelName == "iPhone 6 Plus" {
        // is an iPhone 6 Plus
    }


**Swift 3**

    import UIKit

    public extension UIDevice {

        var modelName: String {
            var systemInfo = utsname()
            uname(&systemInfo)
            let machineMirror = Mirror(reflecting: systemInfo.machine)
            let identifier = machineMirror.children.reduce("") { identifier, element in
                guard let value = element.value as? Int8 , value != 0 else { return identifier     }
                return identifier + String(UnicodeScalar(UInt8(value)))
            }

            switch identifier {
            case "iPod5,1":                                 return "iPod Touch 5"
            case "iPod7,1":                                 return "iPod Touch 6"
            case "iPhone3,1", "iPhone3,2", "iPhone3,3":     return "iPhone 4"
            case "iPhone4,1":                               return "iPhone 4s"
            case "iPhone5,1", "iPhone5,2":                  return "iPhone 5"
            case "iPhone5,3", "iPhone5,4":                  return "iPhone 5c"
            case "iPhone6,1", "iPhone6,2":                  return "iPhone 5s"
            case "iPhone7,2":                               return "iPhone 6"
            case "iPhone7,1":                               return "iPhone 6 Plus"
            case "iPhone8,1":                               return "iPhone 6s"
            case "iPhone8,2":                               return "iPhone 6s Plus"
            case "iPhone9,1", "iPhone9,3":                  return "iPhone 7"
            case "iPhone9,2", "iPhone9,4":                  return "iPhone 7 Plus"
            case "iPhone8,4":                               return "iPhone SE"
            case "iPad2,1", "iPad2,2", "iPad2,3", "iPad2,4":return "iPad 2"
            case "iPad3,1", "iPad3,2", "iPad3,3":           return "iPad 3"
            case "iPad3,4", "iPad3,5", "iPad3,6":           return "iPad 4"
            case "iPad4,1", "iPad4,2", "iPad4,3":           return "iPad Air"
            case "iPad5,3", "iPad5,4":                      return "iPad Air 2"
            case "iPad2,5", "iPad2,6", "iPad2,7":           return "iPad Mini"
            case "iPad4,4", "iPad4,5", "iPad4,6":           return "iPad Mini 2"
            case "iPad4,7", "iPad4,8", "iPad4,9":           return "iPad Mini 3"
            case "iPad5,1", "iPad5,2":                      return "iPad Mini 4"
            case "iPad6,3", "iPad6,4", "iPad6,7", "iPad6,8":return "iPad Pro"
            case "AppleTV5,3":                              return "Apple TV"
            case "i386", "x86_64":                          return "Simulator"
            default:                                        return identifier
            }
        }
    }

    if UIDevice.current.modelName == "iPhone 7" {
        // is an iPhone 7
    }


## Getting Battery Status and Battery Level
    override func viewDidLoad() {
        super.viewDidLoad()
        NotificationCenter.default.addObserver(self, selector: Selector(("batteryStateDidChange:")), name: NSNotification.Name.UIDeviceBatteryStateDidChange, object: nil)
        NotificationCenter.default.addObserver(self, selector: Selector(("batteryLevelDidChange:")), name: NSNotification.Name.UIDeviceBatteryLevelDidChange, object: nil)
    
        // Stuff...
    }
    
    func batteryStateDidChange(notification: NSNotification){
        // The stage did change: plugged, unplugged, full charge...
    }
    
    func batteryLevelDidChange(notification: NSNotification){
        
        let batteryLevel = UIDevice.current.batteryLevel
        if batteryLevel < 0.0 {
            print(" -1.0 means battery state is UIDeviceBatteryStateUnknown")
            return
        }
        
        print("Battery Level : \(batteryLevel * 100)%")
        // The battery's level did change (98%, 99%, ...)
    }


## Identifying the Device and Operating


## Getting the Device Orientation
    UIDevice *deviceInfo = [UIDevice currentDevice];
    int d = deviceInfo.orientation;

`deviceInfo.orientation` returns an UIDeviceOrientation value which is shown as below:

    UIDeviceOrientationUnknown 0
    UIDeviceOrientationPortrait 1
    UIDeviceOrientationPortraitUpsideDown 2
    UIDeviceOrientationLandscapeLeft 3
    UIDeviceOrientationLandscapeRight 4
    UIDeviceOrientationFaceUp 5
    UIDeviceOrientationFaceDown 6

Listening for device orientation changes in a View Controller:

    - (void)viewWillAppear:(BOOL)animated
    {
        [super viewWillAppear:animated];
        [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
        [[NSNotificationCenter defaultCenter] addObserver:self 
                                                 selector:@selector(deviceOrientationDidChange) 
                                                     name:UIDeviceOrientationDidChangeNotification
                                                   object:nil];
    }
 
    -(void)deviceOrientationDidChange
    {
        UIDeviceOrientation orientation = [[UIDevice currentDevice] orientation];
        if (orientation == UIDeviceOrientationPortrait || orientation == UIDeviceOrientationPortraitUpsideDown) {
             [self changedToPortrait];
        } else if (orientation == UIDeviceOrientationLandscapeLeft || orientation == UIDeviceOrientationLandscapeRight) {
             [self changedToLandscape];
        }

    -(void)changedToPortrait
    {
        // Function Body
    }

    -(void)changedToLandscape
    {
        // Function Body
    }
    

 To disable checking for any orientation change:

    - (void)viewWillDisappear:(BOOL)animated {
        [super viewWillDisappear:animated];
        [[UIDevice currentDevice] endGeneratingDeviceOrientationNotifications];
    }




## Getting the Device Battery State


## Using the Proximity Sensor


