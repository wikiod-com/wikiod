---
title: "Platform-specific behaviour"
slug: "platform-specific-behaviour"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Target Platforms

    if(Device.OS == TargetPlatform.Android)
    {

    }
    else if (Device.OS == TargetPlatform.iOS)
    {

    }
    else if (Device.OS == TargetPlatform.WinPhone)
    {

    }
    else if (Device.OS == TargetPlatform.Windows)
    {

    }
    else if (Device.OS == TargetPlatform.Other)
    {

    }


## Removing icon in navigation header in Anroid
[![enter image description here][1]][1]
*Using a small transparent image called empty.png*
    
    public class MyPage : ContentPage
    {
        public Page()
        {
            if (Device.OS == TargetPlatform.Android)
                NavigationPage.SetTitleIcon(this, "empty.png");
        }
    }


  [1]: http://i.stack.imgur.com/9kvuD.png

## Make label's font size smaller in iOS
    
    Label label = new Label
    {
        Text = "text"
    };
    if(Device.OS == TargetPlatform.iOS)
    {
        label.FontSize = label.FontSize - 2;
    }

