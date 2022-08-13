---
title: "Getting started with xamarin"
slug: "getting-started-with-xamarin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Xamarin Studio on OS X
The first step to start Xamarin development on an OS X machine, is to download and install Xamarin Studio Community version from the [official website][1]. A few fields need to be filled to download the installer as shown in the picture below.

[![The download page of Xamarin Studio Community version][2]][2]

The Xamarin Unified installer takes care of identifying and installing all the required non-Xamarin components (e.g. Android SDK) on top of Xamarin.Android, Xamarin.iOS and Xamarin Studio.

> To develop Xamarin.iOS applications, the following prerequisites have to be met: 
> - The latest iOS SDK from the [iOS developer center](https://developer.apple.com/ios/).
> - The latest version of Xcode from the Mac App Store or the [Apple Developer Website][3].
> - Mac OS X Yosemite (10.10) and above

## Installation process ##
Once the prerequisites have been met, run the Xamarin Installer by double clicking the Xamarin logo.

[![Double click the Xamarin logo to start the installation][4]][4]

OS X Gatekeeper may show a dialog asking you for a confirmation to open the downloaded application. Click "open" to proceed.

[![Click "open" to proceed][5]][5]

To start the actual installation process, you must read and accept the Xamarin software license terms. Check the "I agree to license terms" checkbox and make a note of the automatic usage and error reporting request.

[![Accept the license terms to proceed][6]][6]

Next step in the installation is to select the products to install. The items are mostly self-explanatory but [Intel® HAXM](https://software.intel.com/en-us/android/articles/intel-hardware-accelerated-execution-manager) might be unfamiliar to some developers. It stands for Intel® Hardware Accelerated Execution Manager and it makes Android emulation faster.

Products that are already installed on the system are shown but grayed out.
[![Select products to install][7]][7]

After selecting the products, Xamarin Unified installer will automatically download and execute each installer. If Xamarin.Android was selected in the last step, you'll be prompted to select the installation location for the Android SDK. The default location is a safe choice in most cases so press “Continue” to proceed.

[![Select the installation location for Android SDK][8]][8]

Finally, the installer will show a brief summary of what will be downloaded and installed. In this example Xamarin.Android wasn’t installed yet, so it’s shown on the list with other prerequisites.

By clicking “Continue” the download and installation process starts for each product. The installer may ask for a permission to make changes to the system by showing a dialog prompting for the username and password of the current system user. Input the details and click “Ok” to proceed with the installation.

[![Installation process][9]][9]

Once the installation is complete, Xamarin Studio can be launched. The Community edition is free and doesn't require logging in, but to use the Enterprise features an account has to be created and the trial activated.

[![Installation complete][10]][10]

## Next steps ##

- [Hello World (iOS) on Xamarin Studio][11]

  [1]: http://xamarin.com/download
  [2]: http://i.stack.imgur.com/sJFEZ.png
  [3]: https://developer.apple.com/
  [4]: http://i.stack.imgur.com/gcm0g.png
  [5]: http://i.stack.imgur.com/SMmyw.png
  [6]: http://i.stack.imgur.com/Ctzd8.png
  [7]: http://i.stack.imgur.com/BQU15.png
  [8]: http://i.stack.imgur.com/TlpnP.png
  [9]: http://i.stack.imgur.com/V7YVF.png
  [10]: http://i.stack.imgur.com/pV30U.png
  [11]: http://i.stack.imgur.com/dYrzH.png

## Hello World using Xamarin Studio : Xamarin.Forms
After Successfully installing Xamarin Studio on OS X.
It's time for the first Hello World Application.

Hello World  Application: Xamarin.Forms


**What is Xamarin Forms :**

Xamarin.Forms is a new library that enables you to build native UIs for iOS, Android and Windows Phone from a single, shared C# codebase. It provides more than 40 cross-platform controls and layouts which are mapped to native controls at runtime, which means that your user interfaces are fully native

**Step 1:**

Create a new solution.

Click on the "New Solution"
[![Click on the "New Solution"][1]][1]

**Step 2:**
Select Forms App and click Next
[![Select Forms App and click Next][2]][2]

**Step 3:**
Add App name and click Next
[![enter image description here][3]][3]

This is how the project stricture will look like when the solution is created:

[![enter image description here][4]][4]

**App.xaml:**

<!-- language: xml -->

<!-- language: xaml -->
    <?xml version="1.0" encoding="utf-8"?>
    <Application xmlns="http://xamarin.com/schemas/2014/forms" 
        xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
        x:Class="HelloXamarinForms.App">
        <Application.Resources>
            <!-- Application resource dictionary -->
        </Application.Resources>
    </Application>

**App.xaml.cs:**

<!-- language: c# -->
<!-- language: c# -->
    using Xamarin.Forms;
    
    namespace HelloXamarinForms
    {
        public partial class App : Application
        {
            public App()
            {
                InitializeComponent();
    
                MainPage = new HelloXamarinFormsPage();
            }
    
            protected override void OnStart()
            {
                // Handle when your app starts
            }
    
            protected override void OnSleep()
            {
                // Handle when your app sleeps
            }
    
            protected override void OnResume()
            {
                // Handle when your app resumes
            }
        }
    }

**HelloXamarinFormsPage.xaml**

<!-- language: xml -->

<!-- language: xaml -->
    <?xml version="1.0" encoding="utf-8"?>
    <ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
        xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
        xmlns:local="clr-namespace:HelloXamarinForms"
        x:Class="HelloXamarinForms.HelloXamarinFormsPage">

        <Label Text="Welcome to Xamarin Forms!" VerticalOptions="Center"
            HorizontalOptions="Center" />
    </ContentPage>

**HelloXamarinFormsPage.xaml.cs**

<!-- language: c# -->
<!-- language: c# -->
    using Xamarin.Forms;
    
    namespace HelloXamarinForms
    {
        public partial class HelloXamarinFormsPage : ContentPage
        {
            public HelloXamarinFormsPage()
            {
                InitializeComponent();
            }
        }
    }

  [1]: http://i.stack.imgur.com/VO7Pr.png
  [2]: http://i.stack.imgur.com/b81iI.png
  [3]: http://i.stack.imgur.com/glNQW.png
  [4]: http://i.stack.imgur.com/7xzAg.png


