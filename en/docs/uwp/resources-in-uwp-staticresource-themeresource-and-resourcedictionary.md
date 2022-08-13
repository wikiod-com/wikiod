---
title: "Resources in UWP (StaticResource  ThemeResource) and ResourceDictionary"
slug: "resources-in-uwp-staticresource--themeresource-and-resourcedictionary"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

In the new Windows 10 Applications there are many ways to reference a resource inside XAML code or in code behind. First of all you have to declare the resources in some accessible place. The easy way is to declare a `ResourceDictionary` in context, let's say in the current page.

## 1. Resource Dictionary
# Snippet from MainPage.xaml
```XAML

<Page
    x:Class="MyNewApp.MainPage"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    xmlns:local="using:MyNewApp"
    xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
    xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
    mc:Ignorable="d">

    <Page.Resources>
        <!-- Creates a resource dictionary in the page context -->
        <ResourceDictionary>
            <!-- This is a solid color brush resource
                 NOTE: Each resource inside a resource dictionary must have a key -->
            <SolidColorBrush x:Key="ColorRed">Red</SolidColorBrush>
        </ResourceDictionary>
    </Page.Resources>

    <!-- Using ThemeResource in here to access a resource already defined -->
    <Grid Background="{ThemeResource ColorRed}">

    </Grid>
</Page>
```

## 2. Global Resources
Resource dictionaries are accessible only inside the context they were declared, so if we intended to reference resources that are declared in one page context from another page they will not be found. So if we need global resources to be defined like the ones that comes with the framework we do it in App.xaml

# Snippet from App.xaml
```XAML

<Application
    x:Class="MyNewApp.App"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    RequestedTheme="Dark">

    <Application.Resources>
        <ResourceDictionary>
            <SolidColorBrush x:Key="ColorRed">Red</SolidColorBrush>
        </ResourceDictionary>
    </Application.Resources>
</Application>
```

This way we can access `ColorRed` color resource from anywere in our app. But wait, we don't want to infest that little file with all our app's resources! So we do `MergedDictionaries`

## 3. Merged Dictionaries
Almost usually things are a little bit more complex and to support scalability we should split things apart. So we can define various files containing different resources dictionaries, i.e. resources for UI controls' themes, resources for texts and so on, then we merge them all together in App.xaml file.

# Snippet from App.xaml
```XAML
<Application
    x:Class="MyNewApp.App"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    RequestedTheme="Dark">

    <Application.Resources>
        <ResourceDictionary>
            <ResourceDictionary.MergedDictionaries>
                <ResourceDictionary Source="/Assets/Themes/GeneralStyles.xaml"/>
                <ResourceDictionary Source="/Assets/Themes/TemplatedControls.xaml"/>
                <ResourceDictionary Source="/Assets/Strings/Texts.xaml"/>
                <ResourceDictionary Source="/Assets/Strings/ErrorTexts.xaml"/>
            </ResourceDictionary.MergedDictionaries>
        </ResourceDictionary>
    </Application.Resources>
</Application>
```

<br>

You can create a new dictionary file by right clicking on Asset folder [Add -> New Item]

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/mj13u.png

## 4. Accessing Resources
We now need to access to our declared resources, in order to do that from XAML code we use `{ThemeResource ResourceKey}` or `{StaticResource ResourceKey}`

```XAML
to be continued later.
```

