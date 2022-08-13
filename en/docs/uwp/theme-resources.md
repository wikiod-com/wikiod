---
title: "Theme Resources"
slug: "theme-resources"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Syntax
- C# : Application.Current.Resources["yourColorKey"]
- Xaml : {ThemeResource yourColorKey}


## Parameters
| Parameter | Purpose|
| ------ | ------ |
|  `yourColorKey`| A key you give to get a `Color` object back. It differs between C# and Xaml|



UWP allows you to take full control of the advantages of Windows 10. Some of these advantages are graphical, as the Accent color or Dark/Light themes.

To prepare your app to be compatible with these feature, a bunch of premade colors have been implemented in UWP to change with the Accent color of the OS the program runs on, or with the theme choice of the user.

There are two "ways" of doing this :
    
- Diretly in Xaml, using the `Color = {ThemeResource x}` Attribute (or whatever attribute that takes a `Brush`as value, like BorderBrush, Background, etc.)

- In C# Code Behind, by Searching for the color in the Resource directory of the current app. This gives a `Color` object, so if you want to put it in the `Color` property of an object you referenced from your Xaml, you'll need to make a new brush like this :
>`new SolidColorBrush(Application.Current.Resources["yourColorKey"])`

_For a reference of color keys in c#, please consult :_
>https://msdn.microsoft.com/windows/uwp/controls-and-patterns/xaml-theme-resources

## Access to Theme Resources in Xaml
# Snippet from MyExampleFile.xaml
```xml

<TextBlock Foreground="{ThemeResource SystemControlBackgroundAccentBrush}"
         Text="This is a colored textbox that use the Accent color of your Windows 10"/>

<TextBlock Foreground="{ThemeResource SystemControlBackgroundBaseHighBrush}"
         Text="This is a colored textbox that use a color that is readable in both Light and Dark theme"/>
```

## Access to Theme Resources in C#
# Snippet from MyExampleFile.xaml
```xml

<TextBlock x:Name="MyTextBlock"
         Text="This is a TextBlock colored from the code behind"/>
```

# Snippet from MyExampleFile.xaml.cs
```csharp
    // We use the application's Resource dictionary to get the current Accent of your Windows 10
    MyTextBlock.Color = new SolidColorBrush(Application.Current.Resources["SystemAccentColor"]);
```

