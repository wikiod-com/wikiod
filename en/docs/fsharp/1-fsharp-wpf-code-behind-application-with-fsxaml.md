---
title: "1  F# WPF Code Behind Application with FsXaml"
slug: "1--f-wpf-code-behind-application-with-fsxaml"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Most examples found for F# WPF programming seem to deal with the MVVM pattern, and a few with MVC, but there is next to none that shows properly how to get up and running with "good old" code behind.

The code behind pattern is very easy to use for teaching as well as experimentation. It is used in numerous introduction books and learning material on the web. That's why.

These examples will demonstrate how to create a code behind application with windows, controls, images and icons, and more.

## Create a new F# WPF Code Behind Application.
Create an F# console application.

Change the **Output type** of the application to *Windows Application*.

Add the **FsXaml** NuGet package.

Add these four source files, in the order listed here.

MainWindow.xaml

    <Window
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="First Demo" Height="200" Width="300">
        <Canvas>
            <Button Name="btnTest" Content="Test" Canvas.Left="10" Canvas.Top="10" Height="28" Width="72"/>
        </Canvas>
    </Window>

MainWindow.xaml.fs

    namespace FirstDemo
    
    type MainWindowXaml = FsXaml.XAML<"MainWindow.xaml">
    
    type MainWindow() as this =
        inherit MainWindowXaml()

        let whenLoaded _ =
            ()

        let whenClosing _ =
            ()

        let whenClosed _ =
            ()

        let btnTestClick _ =
            this.Title <- "Yup, it works!"

        do
            this.Loaded.Add whenLoaded
            this.Closing.Add whenClosing
            this.Closed.Add whenClosed
            this.btnTest.Click.Add btnTestClick


App.xaml

    <Application xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                 xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml">
        <Application.Resources>
        </Application.Resources>
    </Application>

App.xaml.fs

    namespace FirstDemo
    
    open System
    
    type App = FsXaml.XAML<"App.xaml">
    
    module Main =
    
        [<STAThread; EntryPoint>]
        let main _ =
            let app = App()
            let mainWindow = new MainWindow()
            app.Run(mainWindow) // Returns application's exit code.

Delete the *Program.fs* file from the project.

Change the **Build Action** to *Resource* for the two xaml files.

Add a reference to the .NET assembly **UIAutomationTypes**.

Compile and run.

You can't use the designer to add event handlers, but that's not a problem at all. Simply add them manually in the code behind, like you see with the three handlers in this example, including the handler for the test button.

UPDATE: An alternative and probably more elegant way to add event handlers has been added to FsXaml. You can add the event handler in XAML, same as in C# but you have to do it manually, and then override the corresponding member that turns up in your F# type. I recommend this.

## 3 : Add an icon to a window
It's a good idea to keep all icons and images in one or more folders.

Right click on the project, and use F# Power Tools / New Folder to create a folder named Images.

On disk, place your icon in the new *Images* folder.

Back in Visual Studio, right click on *Images*, and use **Add / Existing Item**, then show **All Files (*.*)** to see the icon file so that you can select it, and then **Add** it.

Select the icon file, and set its **Build Action** to *Resource*.

In MainWindow.xaml, use the Icon attribute like this. Surrounding lines are shown for context.

        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="First Demo" Height="200" Width="300"
        Icon="Images/MainWindow.ico">
    <Canvas>

Before you run, do a Rebuild, and not just a Build. This is because Visual Studio doesn't always put the icon file into the executable unless you rebuild.

It is the window, and not the application, that now has an icon. You will see the icon in the top left of the window at runtime, and you will see it in the task bar. The Task Manager and Windows File Explorer will not show this icon, because they display the application icon rather than the window icon.

## 4 : Add icon to application
Create a text file named AppIcon.rc, with the following content.

    1 ICON "AppIcon.ico"

You will need an icon file named AppIcon.ico for this to work, but of course you can adjust the names to your liking.

Run the following command.

    "C:\Program Files (x86)\Windows Kits\10\bin\x64\rc.exe" /v AppIcon.rc

If you can't find rc.exe in this location, then search for it below **C:\Program Files (x86)\Windows Kits**. If you still can't find it, then download Windows SDK from Microsoft.

A file named AppIcon.res will be generated.

In Visual Studio, open the project properties. Select the **Application** page.

In the text box titled **Resource File**, type *AppIcon.res* (or *Images\AppIcon.res* if you put it there), and then close the project properties to save.

An error message will appear, stating "The resource file entered does not exist. Ignore this. The error message will not reappear.

Rebuild. The executable will then have an application icon, and this shows in File Explorer. When running, this icon will also appear in Task Manager.

## 2 : Add a control
Add these two files in this order above the files for the main window.

**MyControl.xaml**

    <UserControl
                 xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                 xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                 xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
                 xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
                 mc:Ignorable="d" Height="50" Width="150">
        <Canvas Background="LightGreen">
            <Button Name="btnMyTest" Content="My Test" Canvas.Left="10" Canvas.Top="10" Height="28" Width="106"/>
        </Canvas>
    </UserControl>

**MyControl.xaml.fs**

    namespace FirstDemo
    
    type MyControlXaml = FsXaml.XAML<"MyControl.xaml">
    
    type MyControl() =
        inherit MyControlXaml()

The **Build Action** for *MyControl.xaml* must be set to *Resource*.

You will of course later need to add "as this" in the declaration of MyControl, just as done for the main window.

In file **MainWindow.xaml.fs**, in the class for MainWindow, add this line

    let myControl = MyControl()

and add these two lines in the **do**-section of the main window class.

        this.mainCanvas.Children.Add myControl |> ignore
        myControl.btnMyTest.Content <- "We're in business!"


There can be more than one **do**-section in a class, and you are likely to need it when writing lots of code-behind code.

The control has been given a light green background color, so that you can easily see where it is.

Be aware that the control will block the button of the main window from view. It is beyond the scope of these examples to teach you WPF in general, so we won't fix that here.

## How to add controls from third party libraries
If you add controls from third party libraries in a C# WPF project, the XAML file will normally have lines like this one.

    xmlns:xctk="http://schemas.xceed.com/wpf/xaml/toolkit"

This will perhaps not work with FsXaml.

The designer and the compiler accepts that line, but there will probably be an exception at runtime complaining about the 3rd party type not being found when reading the XAML.

Try something like the following instead.

    xmlns:xctk="clr-namespace:Xceed.Wpf.Toolkit;assembly=Xceed.Wpf.Toolkit"

This then is an example of a control that depends on the above.

    <xctk:IntegerUpDown Name="tbInput" Increment="1" Maximum="10" Minimum="0" Canvas.Left="13" Canvas.Top="27" Width="270"/>

The library used in this example is the Extended Wpf Toolkit, available free of charge through NuGet or as installer. If you download libraries through NuGet, then the controls are not available in the Toolbox, but they still show in the designer if you add them manually in XAML, and the properties are available in the Properties pane.

