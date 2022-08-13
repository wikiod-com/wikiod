---
title: "WPF Localization"
slug: "wpf-localization"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Content of controls can be localized using Resource files, just as this is possible in classes. For XAML there is a specific syntax, that is different between a C# and a VB application.

The steps are:

 - For any WPF project: make the resource file public, the default is internal.
 - For C# WPF projects use the XAML provided in the example
 - For VB WPF projects use the XAML provided in the example and change the Custom Tool property to `PublicVbMyResourcesResXFileCodeGenerator`. 
 - To select the Resources.resx file in a VB WPF project:
    - Select the project in solution explorer
    - Select "Show all files"
    - Expand My Project


## Make the resources public
Open the resource file by double clicking it. Change the Access Modifier to "Public".
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/kj0Qu.png

## XAML for VB
    <Window x:Class="MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
        xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
        xmlns:local="clr-namespace:WpfApplication1"
        xmlns:my="clr-namespace:WpfApplication1.My.Resources"
        mc:Ignorable="d"
        Title="MainWindow" Height="350" Width="525">
    <Grid>
        <StackPanel>
            <Label Content="{Binding Source={x:Static my:Resources.MainWindow_Label_Country}}" />
        </StackPanel>
    </Grid>
</Window>


## Properties for the resource file in VB
By default the Custom Tool property for a VB resource file is `VbMyResourcesResXFileCodeGenerator`. However, with this code generator the view (XAML) will not be able to access the resources. To solve this problem add `Public` before the Custom Tool property value.

To select the Resources.resx file in a VB WPF project: 

 - Select the project in solution explorer
 - Select "Show all files"
 - Expand "My Project"

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/9L2Mb.png

## XAML for C#
    <Window x:Class="WpfApplication2.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
        xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
        xmlns:local="clr-namespace:WpfApplication2"
        xmlns:resx="clr-namespace:WpfApplication2.Properties"
        mc:Ignorable="d"
        Title="MainWindow" Height="350" Width="525">
    <Grid>
        <StackPanel>
            <Label Content="{Binding Source={x:Static resx:Resources.MainWindow_Label_Country}}"/>
        </StackPanel>
    </Grid>
</Window>


