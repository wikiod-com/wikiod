---
title: "AppSettings Reader in Xamarin.Forms"
slug: "appsettings-reader-in-xamarinforms"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Reading app.config file in a Xamarin.Forms Xaml project
While each mobile platforms do offer their own settings management api, there are no built in ways to read settings from a good old .net style app.config xml file;
This is due to a bunch of good reasons, notably the .net framework configuration management api being on the heavyweight side, and each platform having their own file system api.

So we built a simple [PCLAppConfig][1] library, nicely nuget packaged for your immediate consumption.

This library makes use of the lovely [PCLStorage][2] library

This example assumes you are developing a Xamarin.Forms Xaml project, where you would need to access settings from your shared viewmodel.

1. Initialize ConfigurationManager.AppSettings on each of your platform project, just after the 'Xamarin.Forms.Forms.Init' statement, as per below:


iOS (AppDelegate.cs)

    global::Xamarin.Forms.Forms.Init();
    ConfigurationManager.Initialise(PCLAppConfig.FileSystemStream.PortableStream.Current);
    LoadApplication(new App());

  [1]: https://www.nuget.org/packages/PCLAppConfig
  [2]: https://www.nuget.org/packages/pclstorage

Android (MainActivity.cs)

    global::Xamarin.Forms.Forms.Init(this, bundle);
    ConfigurationManager.Initialise(PCLAppConfig.FileSystemStream.PortableStream.Current);
    LoadApplication(new App());

UWP / Windows 8.1 / WP 8.1 (App.xaml.cs)

    Xamarin.Forms.Forms.Init(e);
    ConfigurationManager.Initialise(PCLAppConfig.FileSystemStream.PortableStream.Current);


2. Add an app.config file to your shared PCL project, and add your appSettings entries, as you would do with any app.config file


    <configuration>
        <appSettings>
            <add key="config.text" value="hello from app.settings!" />
        </appSettings>
    </configuration>

3. Add this PCL app.config file **as a linked file** on all your platform
    projects. For android, make sure to set the build action to
    **'AndroidAsset'**, for UWP set the build action to **'Content'**

4. Access your setting:
`ConfigurationManager.AppSettings["config.text"];`
    



