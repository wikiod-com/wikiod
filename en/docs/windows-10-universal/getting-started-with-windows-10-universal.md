---
title: "Getting started with windows-10-universal"
slug: "getting-started-with-windows-10-universal"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a new project (C# / XAML)
After launching Visual Studio 2015, go to `File → New → Project`. In the *New Project* dialog box, browse in the templates tree to `Visual C# → Windows → Universal` and select `Blank App (Universal Windows)`.

Next, we need to fill the form to describe the Application:
1. **Name**: this is the name of the application which will be displayed to the user. Set it to `HelloWorld` or use a custom title.
2. **Location**: indicates where the project will be stored
3. **Solution Name**: this is a kind of container of projects which groups several projects related to the same application (for example a solution could be composed of a UI project and a model project). You can put the same `Name` as your initial project.

[![New Project dialog][1]][1]

# Content of the default project

You will obtain a project with the following files:

[![Project files][2]][2]

1. **Package.appxmanifest**: describes properties of your application. It contains some UI settings such as its disaply name, its logo, the supported rotations. And it also contains technical settings such as the entry point of the application (wich is the `App` class by default). Finally, it also list authorizations that are required by your application in the *Capabilities* tab; for example if your want to use the webcam in your application you will have to check the corresponding capabilities.
2. **App.xaml / App.xaml.cs**: the `App` class is the default entry point of your application. The xaml files can hold resources shared across the whole application such as styles setting or instance of a class that you want to share such as a ViewModel locator. The code-behind files contains all the startup code of the application. By default, it implements the `OnLaunched` method which is invoked by the end user. It initializes the window and navigate to the first page of the application (by default the `MainPage` class).
3. **MainPage.xaml / MainPage.xaml.cs**: this is the initial page of our application. It contains only an empty Grid which is a layout control.


# Modify the view

Open the `MainPage.xaml` and replace the Grid control with

    <Grid Background="{ThemeResource ApplicationPageBackgroundThemeBrush}">
            <Grid.ColumnDefinitions>
                <ColumnDefinition Width="auto" />
                <ColumnDefinition Width="*" />
            </Grid.ColumnDefinitions>
            <Button Click="Button_Click">Say Hello !</Button>
            <TextBlock Grid.Column="1"
                       VerticalAlignment="Center"
                       x:Name="myText"
                       Text="Click the button." />
    </Grid>

This will create a grid with two columns. The first column as a width set to `auto` which means that it will automatically be set in function of the size of its children. The second column will stretch to fill the remaining space in the window.

This grid contains two elements:

 - a `Button` that lies in the first column. The click event is bind to the method `Button_Click` on the code-behind and its caption Text is *"Say Hello!"*.
 - a `TextBlock` that lies in the second column. It's text is set to *"Click the button."*. And we have set a name to this control with the help of the attribute `x:Name`. This is required to be able to use the control in the code-behind.

In the `MainPage.xaml.cs`, add the following code:

    private void Button_Click(object sender, RoutedEventArgs e)
    {
        this.myText.Text = "Hello World!";
    }

This is the method that will be called when the user clicks (or taps) the button. And it will updates the `TextBlock` and set its text to *"Hello World!"*.


# Running the application

To run the application, you can use the menu `Debug → Start Debugging` or the shortcut `F5`. By default, it will run the application on your Local Machine.

  [1]: http://i.stack.imgur.com/iOVLQ.jpg
  [2]: http://i.stack.imgur.com/XFxb2.jpg

## Installation or Setup
If you don't have Visual Studio 2015, go to https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx and select Visual Studio Community. It is a free version that have all the required capabilities to develop Windows 10 application. You can install the Enterprise Edition if you have the required license. Then launch the installer.

If you already have Visual Studio 2015 installed, in the `Windows Control Panel`, go to *Program and Features* and in the list right click on *Microsoft Visual Studi0 2015* (this name can change in function of the installed version of Visual Studio 2015) and select *Change*.

In the Visual Studio Install dialog, click on `Modify` and under *Windows and Web Development* install all the *Universal Windows App Development Tools*. It will install, the Windows 10 SDK, a Windows 10 mobile emulator (powered by Hyper-V) and the Visual Studio required to create Windows 10 applications.

[![enter image description here][1]][1]

When the installation is finished, you can create a new Windows 10 project.


  [1]: http://i.stack.imgur.com/o2yBf.jpg

System Requirements:
Windows 10

Visual Studio 2015( Universal Windows App Development Tools feature)

Instructions to enable Universal Windows App Development Tools feature during installation during visual studio 2015.

i.During installation one need to select customize and need to check   Universal Windows App Development Tools.

ii.If you already have Visual Studio 2015 , you can now add these tools to your existing Visual Studio installation. You can run the installer, or open Programs and Features from Control Panel, select Visual Studio and click Change. Then in setup, click Modify and select the Tools for Universal Windows Apps. 

iii.After installing tools one can create project using  new project templates(Windows->Universal) in Visual Studio 2015 in a language of your choice – C#, VB, C++, or JavaScript.



To start with Windows 10 Universal app development you need to have Visual Studio 2015 on a Windows 10 computer. You can buy Windows 10 from [here][1]. You can download Visual Studio 2015 from [here][2]. You can use Community, Professional or Enterprise versions of Visual Studio. Some features will be limited in the Express version.

You can find a detailed guide how to install Windows 10 [here][3].

When installing Visual Studio choose Custom and under 'Windows and Web Development' select 'Universal Windows App Development Tools' from the feature selection.[![enter image description here][4]][4]
    
When finished you are ready to develop Universal Windows apps!

  [1]: https://www.microsoft.com/en-us/software-download/windows10
  [2]: https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx
  [3]: http://www.howtogeek.com/197559/how-to-install-windows-10-on-your-pc/
  [4]: http://i.stack.imgur.com/FbqIO.png

