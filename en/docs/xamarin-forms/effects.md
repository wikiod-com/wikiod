---
title: "Effects"
slug: "effects"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Effects simplifies platform specific customizations. When there is a need to modify a Xamarin Forms Control's properties, Effects can be used. When there is a need to override the Xamarin Forms Control's methods, Custom renderers can be used

## Adding platform specific Effect for an Entry control
 1. Create a new Xamarin Forms app using PCL File -> New Solution -> Multiplatform App -> Xamarin Forms -> Forms App; Name the project as `EffectsDemo`
 2. Under the iOS project, add a new `Effect` class that inherits from `PlatformEffect` class and overrides the methods `OnAttached`, `OnDetached` and `OnElementPropertyChanged` Notice the two attributes `ResolutionGroupName` and `ExportEffect`, these are required for consuming this effect from the PCL/shared project. 
 - `OnAttached` is the method where the logic for customization goes in
 - `OnDetached` is the method where the clean up and de-registering happens
 - `OnElementPropertyChanged` is the method which gets triggered upon property changes of different elements. To identify the right property, check for the exact property change and add your logic. In this example, `OnFocus` will give the `Blue` color and `OutofFocus` will give `Red` Color

        using System;
        using EffectsDemo.iOS;
        using UIKit;
        using Xamarin.Forms;
        using Xamarin.Forms.Platform.iOS;

        [assembly: ResolutionGroupName("xhackers")]
        [assembly: ExportEffect(typeof(FocusEffect), "FocusEffect")]
        namespace EffectsDemo.iOS
        {
        public class FocusEffect : PlatformEffect
        {
        public FocusEffect()
        {
        }
        UIColor backgroundColor;
        protected override void OnAttached()
        {
            try
            {
                Control.BackgroundColor = backgroundColor = UIColor.Red;
            }
            catch (Exception ex)
            {
                Console.WriteLine("Cannot set attacked property" + ex.Message);
            }
        }

        protected override void OnDetached()
        {
            throw new NotImplementedException();
        }

        protected override void OnElementPropertyChanged(System.ComponentModel.PropertyChangedEventArgs args)
        {
            base.OnElementPropertyChanged(args);

            try
            {
                if (args.PropertyName == "IsFocused")
                {
                    if (Control.BackgroundColor == backgroundColor)
                    {
                        Control.BackgroundColor = UIColor.Blue;
                    }
                    else
                    {
                        Control.BackgroundColor = backgroundColor;
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Cannot set property " + ex.Message);
            }
        }
    }
}

 3. To Consume this effect in the application, Under the `PCL` project, create a new class named `FocusEffect` which inherits from `RoutingEffect`. This is essential to make the PCL instantiate the platform specific implementation of the effect. Sample code below:

        using Xamarin.Forms;
        namespace EffectsDemo
        {
            public class FocusEffect : RoutingEffect
            {
                public FocusEffect() : base("xhackers.FocusEffect")
                {
                }
            }
        }

 4. Add the effect to `Entry` control in the XAML

        <?xml version="1.0" encoding="utf-8"?>
        <ContentPage xmlns="http://xamarin.com/schemas/2014/forms" xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml" xmlns:local="clr-namespace:EffectsDemo" x:Class="EffectsDemo.EffectsDemoPage">
        <StackLayout Orientation="Horizontal" HorizontalOptions="Center" VerticalOptions="Center">
        <Label Text="Effects Demo" HorizontalOptions="StartAndExpand" VerticalOptions="Center" ></Label>
        <Entry Text="Controlled by effects" HorizontalOptions="FillAndExpand" VerticalOptions="Center">
            <Entry.Effects>
                <local:FocusEffect>
                </local:FocusEffect>
            </Entry.Effects>
        </Entry>
        </StackLayout>
        </ContentPage>
[![Effects Added for iOS version][1]][1]


[![Effects ignored for Droid version][2]][2]

Since the Effect was implemented only in iOS version, when the app runs in `iOS Simulator` upon focusing the `Entry` background color changes and nothing happens in `Android Emulator` as the `Effect` wasn't created under `Droid` project

  [1]: https://i.stack.imgur.com/96stB.gif
  [2]: https://i.stack.imgur.com/dDDuY.gif

