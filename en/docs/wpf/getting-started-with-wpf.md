---
title: "Getting started with wpf"
slug: "getting-started-with-wpf"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World application
To create and run new WPF project in Visual Studio:
1. Click **File → New → Project**

[![New project][1]][1]

2. Select template by clicking **Templates → Visual C# → Windows → WPF Application** and press <kbd>OK</kbd>:

[![enter image description here][2]][2]

3. Open **MainWindow.xaml** file in *Solution Explorer* (if you don't see *Solution Explorer* window, open it by clicking **View → Solution Explorer**):

[![enter image description here][3]][3]

4. In the *XAML* section (by default below *Design* section) add this code


    <TextBlock>Hello world!</TextBlock>

inside `Grid` tag:

[![enter image description here][4]][4]

Code should look like:

    <Window x:Class="WpfApplication1.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
            xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
            xmlns:local="clr-namespace:WpfApplication1"
            mc:Ignorable="d"
            Title="MainWindow" Height="350" Width="525">
        <Grid>
            <TextBlock>Hello world!</TextBlock>
        </Grid>
    </Window>

5. Run the application by pressing <kbd>F5</kbd> or clicking menu **Debug → Start Debugging**. It should look like:

[![enter image description here][5]][5]


  [1]: http://i.stack.imgur.com/qsbmx.png
  [2]: http://i.stack.imgur.com/nj6zZ.png
  [3]: http://i.stack.imgur.com/3E0jz.png
  [4]: http://i.stack.imgur.com/hX4uf.png
  [5]: http://i.stack.imgur.com/LmANq.png

