---
title: "File name qualifiers"
slug: "file-name-qualifiers"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Qualifiers are used in this common format:  
-
_**Files**_: `filename.qualifier-value.ext`  
_**~ multiple qualifiers:**_ `filename.qualifier1-value1_qualifier2-value2_....ext`

_**Qualified folders:**_ `qualifier-value`  
_**~ multiple qualifiers:**_ `qualifier1-value1_qualifier2-value2_...`

Qualifiers are listed bellow, they are used in the format described above
-
| Qualifier | Usage | Values |
| ------ | ------ | ------ |
| Lang / Language | Specifies a language, region or both. | `XX-XX`, or `XX` values in BCP-47 |
| Scale | Qualifies the device scale factor. | *Commonly* 100 / 125 / 150 / 200 / 400 |
| DeviceFamily | Specifies the device type. | Mobile / Team / Desktop / IoT
| Contrast | Specifies the contrast theme type. | Standard / High / Black / White |
| HomeRegion | Specifies user's home region. | Any ISO3166-1 alpha2 or numeric code |
| TargetSize | Gives the smallest image larger than need. | Any positive integer. |
| LayoutDir | Specifies a layout direction. | RTL / LTR / TTBRTL / TTBLTR |
| Config | Qualifies for `MS_CONFIGURATION_ATTRIBUTE_VALUE`. | The value of environment config. |
| DXFL* | Specifies a DirectX feature level. | DX9 / DX10 / DX11 |
\* Also used as **DXFeatureLevel**.

**Some notes to keep in mind:**
-
 - `HomeRegion` won't accept groupings or unions.
 - `TargetSize` and `Scale` cannot be used together.

## Using different views for device types
You can qualify a whole folder folder for a specific device type, its files will override the ones outside it on that device:  

    / DeviceFamily-Mobile
        PageOfEden.xaml
        MainPage.xaml
    MainPage.xaml
    MainPage.xaml.cs
    PageOfEden.xaml
    PageOfEden.xaml.cs

Files inside the qualifying folder won't need qualifiers.

## Default asset scaling qualifiers
If you browse your app's *Assets* folder you will notice that all resources are qualified by their scales (As you are required to put seperate files for each scaling in the package manifest).

    SplashScreen.scale-100.png
    SplashScreen.scale-125.png
    SplashScreen.scale-150.png
    SplashScreen.scale-200.png

## Using the TargetSize qualifier
Let's assume we have an *Image* element using a square image named `Picture.png`.  
We can use different files for each dimension set for the element.

    Picture.TargetSize-16.png
    Picture.TargetSize-32.png
    Picture.TargetSize-128.png

Now if we set the `Height` or `Width` of our *Image* to 16px, it will use `Picture.TargetSize-16.png` as a source. Now if we set the dimensions to 20px, there is no image matching the exact dimensions, so it will use `Picture.TargetSize-32.png`, as it's the nearest image larger than our needs. Dimensions higher than 128 will use `Picture.TargetSize-128.png`.

