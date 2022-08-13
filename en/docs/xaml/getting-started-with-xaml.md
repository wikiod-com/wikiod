---
title: "Getting started with xaml"
slug: "getting-started-with-xaml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The easiest way to get writing your first XAML is to install Microsoft Visual Studio. This is avaliable free from Microsoft. 

Once installed you can create a new project, of type WPF Application, either with a VB.NET or C# code. 

This is similar to windows forms in the sense that you have a series of windows, the main difference being that these windows are written in XAML and are much more responsive to different devices. 

Still needs improvment.

## Hello World
Here is a simple example of an XAML page in WPF. It consists of a `Grid`, a `TextBlock` and a `Button` - the most common elements in XAML.

    <Window x:Class="FirstWpfApplication.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
            xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
            mc:Ignorable="d"
            Title="MainWindow" 
            Height="350"
            Width="525">
        <Grid>
            <TextBlock Text="Welcome to XAML!"
                       FontSize="30"
                       Foreground="Black"
                       HorizontalAlignment="Center"
                       VerticalAlignment="Center"/>
            
            <Button Content="Hello World!"
                    Background="LightGray"
                    Foreground="Black"
                    FontSize="25"
                    Margin="0,100,0,0"
                    VerticalAlignment="Center"
                    HorizontalAlignment="Center"/>
        </Grid>
    </Window>

| Syntax | Description |
| ------ | ------ |
|`<Window>`|The root container which hosts the content that visualizes data and enable users to interact with it. A WPF window is a combination of an XAML (.xaml) file, where the element is the root, and a CodeBehind (.cs) file.|
|`<Grid>`|A layout panel that arranges its child elements in a tabular structure of rows and columns. |
|`<TextBlock>`|Provides a lightweight control for displaying string text in its Text property or Inline flow content elements, such as Bold, Hyperlink, and InlineUIContainer, in its Inlines property.|
|`<Button>`|Represents a button control which reacts with the user click on it.|

| Property | Description |
| ------ | ------ |
|`Title`|Gets or sets the title of a window.|
|`Height`|Gets or sets the height of an element.|
|`Width`|Gets or sets the width of an element.|
|`Text`|Gets or sets the text content of a text element.|
|`FontSize`|Gets or sets the top-level font size for the text.|
|`Background`|Gets or sets the brush color that paints the background of an element.|
|`Foreground`|Gets or sets the brush color that paints the font of a text in an element.|
|`Margin`|Gets or sets the value that describes the outer space between an element and the others.|
|`HorizontalAlignment`|Gets or sets the horizontal alignment characteristics applied to the element when it is composed within a parent element, such as a panel or items control.|
|`VerticalAlignment`|Gets or sets the vertical alignment characteristics applied to the element when it is composed within a parent element such as a panel or items control.|


