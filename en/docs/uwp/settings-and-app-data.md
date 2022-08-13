---
title: "Settings and app data"
slug: "settings-and-app-data"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Store and retrieve settings
UWP applications can easily store simple settings in a key/value store locally or even in the cloud so your application or a game can share settings between different user's devices.

Following data types can be used for settings:

 - UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Single, Double
 - Boolean
 - Char16, String
 - DateTime, TimeSpan
 - GUID, Point, Size, Rect

Start by retrieving the local and/or roaming data container.

    Windows.Storage.ApplicationDataContainer localSettings = Windows.Storage.ApplicationData.Current.LocalSettings;
    Windows.Storage.ApplicationDataContainer roamingSettings = Windows.Storage.ApplicationData.Current.RoamingSettings;

To create or write a setting, use [ApplicationDataContainer.Values][1] property to access the settings in the data container. For example lets create a local setting named `FontSize` with an `int` value `10` and roaming setting `Username` with a `string` value `Bob`.

    localSettings.Values["FontSize"] = 10;
    roamingSettings.Values["Username"] = "Bob";

To retrieve the setting, use the same [ApplicationDataContainer.Values][1] property that you used to create the setting.

    int fontSize = localSettings["FontSize"];
    string username = roamingSettings["Username"];

Good practice is to check if a setting exists before retrieving it.

    if (localSettings.Values.ContainsKey("FontSize"))
        int fontSize = localSettings["FontSize"];
    
    if (roamingSettings.Values.ContainsKey("Username"))
        string username = roamingSettings["Username"];

Roaming settings have size quota. Use [RoamingStorageQuota][2] property go get it.

You can find more about settings, their limits and code examples on [MSDN][3].


  [1]: https://msdn.microsoft.com/library/windows/apps/br241615
  [2]: https://msdn.microsoft.com/library/windows/apps/br241625
  [3]: https://msdn.microsoft.com/en-us/windows/uwp/app-settings/store-and-retrieve-app-data

## Save data to application cache
The [ApplicationData.Current.LocalFolder][1] api allows us to get access to the application cache :

    var file = await ApplicationData.Current.LocalFolder.CreateFileAsync("myFile.dat", CreationCollisionOption.ReplaceExisting);


The [FileIO][2] class contains a set of utility methods to easily add data to a file :

    await FileIO.WriteBytesAsync(file, array);
    await FileIO.AppendTextAsync(file, "text");
    await FileIO.WriteBufferAsync(file, iBuffer);


  [1]: https://msdn.microsoft.com/en-us/library/windows/apps/xaml/br241587(v=win.10).aspx?appid=dev14idef1&l=en-us&k=k(windows.storage.applicationdata)%3Bk(targetframeworkmoniker-.netcore,version%3Dv5.0)%3Bk(devlang-csharp)&rd=true
  [2]: https://msdn.microsoft.com/en-us/library/windows/apps/xaml/hh701440(v=win.10).aspx?appid=dev14idef1&l=en-us&k=k(windows.storage.fileio)%3Bk(targetframeworkmoniker-.netcore,version%3Dv5.0)%3Bk(devlang-csharp)&rd=true

