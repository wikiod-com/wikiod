---
title: "Device Families"
slug: "device-families"
draft: false
images: []
weight: 9905
type: docs
toc: true
---

## DeviceFamily specific code
In general, UWP is used for making a single application that runs on Windows 10 across many different devices. However, it is also possible to make code tailored to specific devices. You can achieve this in several different ways.

**Different XAML Layout**

If you want to use a specific layout on for a certain "device family", you can do this by creating a new XAML Page item with the same name as the default XAML file, with a suffix to indicate the device family you are targeting. Then you'll have *MainPage.xaml* for all devices and *MainPage.DeviceFamily-[specific family].xaml* just for one specific family, which will overwrite the default layout, see below:

[![Image 1][1]][1]

If you want to do this for lots of files, you can make a folder with name *DeviceFamily-[specific family]* and put all XAML pages into it, but now with exactly with the same name as the default XAML file (see below). In both examples, all pages would share the same code-behind file, so the functionality is identical, but the layout is tailored to specific screen sizes.

[![Image 2][2]][2]

**Code for specific family**

If you want to run part of your code-behind or your ViewModel on a specific device family only, you can use the `DeviceFamily` property from the `AnalyticsVersionInfo` class.


    AnalyticsVersionInfo avi = AnalyticsInfo.VersionInfo;
    var deviceFamily = avi.DeviceFamily;
    
    if(deviceFamily == "Windows.Mobile")
    {
       Console.WriteLine("You're on mobile device right now.");
    }
    else if(deviceFamily == "Windows.Desktop")
    {
       Console.WriteLine("You're on desktop");
    }
    else if(deviceFamily == "Windows.IoT")
    {
       Console.WriteLine("You're on IoT");
    }
    //....


  [1]: http://i.stack.imgur.com/zNIi7.png
  [2]: http://i.stack.imgur.com/XO2Og.png

## Get current device family
Here a simple portable way to get the current device family:

    /// <summary>
    /// All the device families 
    /// </summary>
    public enum DeviceFamily
    {
        Desktop,
        Mobile,
        Iot,
        Xbox,
    }

    /// <summary>
    /// The helper to get the current device family
    /// </summary>
    public static class DeviceFamilyHelper
    {
        /// <summary>
        /// Return the family of the current device
        /// </summary>
        /// <returns>the family of the current device</returns>
        public static DeviceFamily GetDeviceFamily()
        {
            switch(ResourceContext.GetForCurrentView().QualifierValues["DeviceFamily"].ToLowerInvariant())
            {
                case "mobile":  return DeviceFamily.Mobile;    
                case "xbox":    return DeviceFamily.Xbox;
                case "iot":     return DeviceFamily.Iot;
                default:        return DeviceFamily.Desktop;
            }
        }
    }

## Detect if an API contract is supported
Depending on the device/release version of the system, some API may not be available.
You can check which contract is supported by using [ApiInformation.IsApiContractPresent()][1]

For example, this will return true on phone devices and false on the others

    ApiInformation.IsApiContractPresent(typeof(CallsPhoneContract).FullName, 1)


The contract where an API belong is available at the bottom the API page on the MSDN or the global list is available from the [API contract page][2].


  [1]: https://msdn.microsoft.com/en-us/library/windows/apps/xaml/windows.foundation.metadata.apiinformation.isapicontractpresent.aspx
  [2]: https://msdn.microsoft.com/en-us/library/windows/apps/dn706135.aspx

