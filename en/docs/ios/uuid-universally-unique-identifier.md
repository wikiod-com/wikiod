---
title: "UUID (Universally Unique Identifier)"
slug: "uuid-universally-unique-identifier"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

To save the UUID we can use [SSKeychainUtility][1]. Example can be found on Github page


  [1]: https://github.com/soffes/SSKeychain

## Apple's IFA vs. IFV (Apple Identifier for Advertisers vs. Identifier for Vendors)
 - You can use the IFA for measuring ad clicks and the IFV for measuring
   app installs. 
 - IFA has built-in privacy mechanisms that make it
   perfect for advertising. In contrast, the IFV is for developers to
   use internally to measure users who install their apps.

**[IFA][1]** 
 - ASIdentifierManager class provides
    - **advertisingIdentifier: UUID**: An alphanumeric string unique to each device, used only for serving advertisements.

    - **isAdvertisingTrackingEnabled**: A Boolean value that indicates whether the user has limited ad tracking.
 
**[IFV][2]**
 - ASIdentifierManager class provides
    - **identifierForVendor: UUID**: An alphanumeric string that uniquely identifies a device to the app’s vendor.


   


Find your device IFA and IFV [here][3].


  [1]: https://developer.apple.com/reference/adsupport/asidentifiermanager
  [2]: https://developer.apple.com/reference/uikit/uidevice#//apple_ref/occ/instp/UIDevice/identifierForVendor
  [3]: https://itunes.apple.com/us/app/my-tune-device/id1100377074?ls=1&mt=8

## Generating UUID
# Random UUID

## Swift

    func randomUUID() -> NSString{
        return NSUUID.UUID().UUIDString()
    }

## Objective-C


    + (NSString *)randomUUID {
        if(NSClassFromString(@"NSUUID")) { // only available in iOS >= 6.0
            return [[NSUUID UUID] UUIDString];
        }
        CFUUIDRef uuidRef = CFUUIDCreate(kCFAllocatorDefault);
        CFStringRef cfuuid = CFUUIDCreateString(kCFAllocatorDefault, uuidRef);
        CFRelease(uuidRef);
        NSString *uuid = [((__bridge NSString *) cfuuid) copy];
        CFRelease(cfuuid);
        return uuid;
    }

## Identifier for vendor
<!-- if version [gte iOS 6] -->

Within a single line, we can get an UUID like below:

## Swift

    let UDIDString = UIDevice.currentDevice().identifierForVendor?.UUIDString

## Objective-C

    NSString *UDIDString = [[[UIDevice currentDevice] identifierForVendor] UUIDString];

The `identifierForVendor` is an unique identifier that stays the same for every app of a single vendor on a single device, unless all of the vendor's apps are deleted from this device. See [Apple's documentation][1] about when this `UUID` changes.

<!-- end version if -->




  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIDevice_Class/#//apple_ref/occ/instp/UIDevice/identifierForVendor

## Create UUID String for iOS devices
Here we can create `UUID String` with in one line.

Represents UUID strings, which can be used to uniquely identify types, interfaces, and other items.

# Swift 3.0

    print(UUID().uuidString)

It is very useful for identify multiple devices with unique id.  

