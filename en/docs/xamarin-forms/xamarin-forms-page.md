---
title: "Xamarin.Forms Page"
slug: "xamarinforms-page"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## TabbedPage
A TabbedPage is similar to a NavigationPage in that it allows for and manages simple
navigation between several child Page objects. The difference is that generally speaking, each
platform displays some sort of bar at the top or bottom of the screen that displays most, if not
all, of the available child Page objects.
In Xamarin.Forms applications, a TabbedPage is generally useful when you have a small
predefined number of pages that users can navigate between, such as a menu or a simple
wizard that can be positioned at the top or bottom of the screen.

**XAML**

[![enter image description here][1]][1]


**Code**

    var page1 = new ContentPage {
    Title = "Tab1",
    Content = new Label {
    Text = "I'm the Tab1 Page",
    HorizontalOptions = LayoutOptions.Center,
    VerticalOptions = LayoutOptions.Center
    }
    };
    var page2 = new ContentPage {
    Title = "Tab2",
    Content = new Label {
    Text = "I'm the Tab2 Page",
    HorizontalOptions = LayoutOptions.Center,
    66
    VerticalOptions = LayoutOptions.Center
    }
    };
    var tabbedPage = new TabbedPage {
    Children = { page1, page2 }
    };

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/BbYAQ.png
  [2]: http://i.stack.imgur.com/UTaHv.png

## ContentPage
ContentPage: Displays a single View.

**XAML**

    <?xml version="1.0" encoding="utf-8" ?>
    <ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
    xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
    x:Class="XamlBasics.SampleXaml">
    <Label Text="This is a simple ContentPage"
    HorizontalOptions="Center"
    VerticalOptions="Center" />
    </ContentPage>

**Code**

    var label = new Label {
    Text = "This is a simple ContentPage",
    HorizontalOptions = LayoutOptions.Center,
    VerticalOptions = LayoutOptions.Center
    };
    var contentPage = new ContentPage {
    Content = label
    };
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/bPwVY.png

## MasterDetailPage
MasterDetailPage: Manages two separate Pages (panes) of information.

**XAML**

    <?xml version="1.0" encoding="utf-8" ?>
    <MasterDetailPage xmlns="http://xamarin.com/schemas/2014/forms"
    xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
    x:Class="XamlBasics.SampleXaml">
    <MasterDetailPage.Master>
    <ContentPage Title = "Master" BackgroundColor = "Silver">
    <Label Text="This is the Master page."
    TextColor = "Black"
    HorizontalOptions="Center"
    VerticalOptions="Center" />
    </ContentPage>
    </MasterDetailPage.Master>
    <MasterDetailPage.Detail>
    <ContentPage>
    <Label Text="This is the Detail page."
    HorizontalOptions="Center"
    VerticalOptions="Center" />
    </ContentPage>
    </MasterDetailPage.Detail>
    </MasterDetailPage>

**Code**

    var masterDetailPage = new MasterDetailPage {
    Master = new ContentPage {
    Content = new Label {
    Title = "Master",
    BackgroundColor = Color.Silver,
    
    TextColor = Color.Black,
    Text = "This is the Master page.",
    HorizontalOptions = LayoutOptions.Center,
    VerticalOptions = LayoutOptions.Center
    }
    },
    Detail = new ContentPage {
    Content = new Label {
    Title = "Detail",
    Text = "This is the Detail page.",
    HorizontalOptions = LayoutOptions.Center,
    VerticalOptions = LayoutOptions.Center
    }
    }
    };
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/J8SLX.png

