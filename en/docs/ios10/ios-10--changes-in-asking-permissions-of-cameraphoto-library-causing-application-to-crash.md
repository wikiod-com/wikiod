---
title: "iOS 10 - Changes in asking permissions of Camera,Photo Library causing application to crash"
slug: "ios-10---changes-in-asking-permissions-of-cameraphoto-library-causing-application-to-crash"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

In iOS 10 Apple is extending the scope of privacy control.
User need to ask the use permission before access the user private data in following key :Calendar,Contact,Reminder,Photo,Bluetooth Sharing,Microphone , Camera ,Location,Heath,HomeKit ,MediaLibrary,Motion,CallKit,Speech Recognition, SiriKit,TV Provider User need to declare in info.plist file to access any of private data.

## IOS 10 Permission required for accessing Camera, Photo library ....
> iOS 10 Now Requires User Permission to Access Media Library, Photos,
> Camera and other Hardware like these. The solution for this is to add
> their keys into info.plist with description for user that how we are
> using their data , iOS already required permissions to access microphone, camera, and media library earlier (iOS6, iOS7), but since iOS10 the apps will crash if you don't provide the description why you are asking for the permission.

**There is a list of all Cocoa Keys that you can specify in your Info.plist file**

**Photo :**

    Key       :  Privacy - Photo Library Usage Description    
    Value   :  $(PRODUCT_NAME) photo use

**Microphone :**

    Key        :  Privacy - Microphone Usage Description    
    Value    :  $(PRODUCT_NAME) microphone use
**Camera :**

    Key       :  Privacy - Camera Usage Description   
    Value   :  $(PRODUCT_NAME) camera use






https://developer.apple.com/library/content/documentation/General/Reference/InfoPlistKeyReference/Articles/CocoaKeys.html][1]


  [1]: https://developer.apple.com/library/content/documentation/General/Reference/InfoPlistKeyReference/Articles/CocoaKeys.html

## Permissions needed for privacy Keys in iOS 10
**Calendar :**
Key      : Privacy - Calendars Usage Description    
Value    : $(PRODUCT_NAME) calendar events

**Reminder :**
Key      :   Privacy - Reminders Usage Description    
Value    :   $(PRODUCT_NAME) reminder use

**Contact :**
Key       :   Privacy - Contacts Usage Description     
Value     :   $(PRODUCT_NAME) contact use

**Photo :**
Key       :  Privacy - Photo Library Usage Description    
Value     :  $(PRODUCT_NAME) photo use

**Bluetooth Sharing :**
Key       :  Privacy - Bluetooth Peripheral Usage Description     
Value     :  $(PRODUCT_NAME) Bluetooth Peripheral use

**Microphone :**
Key        :  Privacy - Microphone Usage Description    
Value      :  $(PRODUCT_NAME) microphone use

**Camera :**
Key       :  Privacy - Camera Usage Description   
Value     :  $(PRODUCT_NAME) camera use

**Location :**
Key      :  Privacy - Location Always Usage Description   
Value    :  $(PRODUCT_NAME) location use

Key      :  Privacy - Location When In Use Usage Description   
Value    :  $(PRODUCT_NAME) location use

**Heath :**
Key      :  Privacy - Health Share Usage Description   
Value    :  $(PRODUCT_NAME) health share use

Key      :  Privacy - Health Update Usage Description   
Value    :  $(PRODUCT_NAME) health update use

**HomeKit :**
Key      :  Privacy - HomeKit Usage Description   
Value    :  $(PRODUCT_NAME) home kit use

**Media Library :**
Key      :  Privacy - Media Library Usage Description   
Value    :  $(PRODUCT_NAME) media library use

**Motion :**
Key      :  Privacy - Motion Usage Description   
Value    :  $(PRODUCT_NAME) motion use

**Speech Recognition :**
Key      :  Privacy - Speech Recognition Usage Description   
Value    :  $(PRODUCT_NAME) speech use

**SiriKit  :** 
Key      :  Privacy - Siri Usage Description  
Value    :  $(PRODUCT_NAME) siri use

**TV Provider :** 
Key      :  Privacy - TV Provider Usage Description   
Value    :  $(PRODUCT_NAME) tvProvider use

> If you don't providing the privacy key in  Info.plist, then app will
> crash. Logs are like this :
> 
> The app has crashed because it attempted to access privacy-sensitive
> data without a usage description. The app's Info.plist must contain an
> **NSCalendarUsageDescription** key with a string value explaining to the user how the app user how the app uses this data.

