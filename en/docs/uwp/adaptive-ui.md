---
title: "Adaptive UI"
slug: "adaptive-ui"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Use the AdaptiveTrigger to change the UI layout
The UWP applications can run in windowed mode and on several devices. They can be displayed on a wide range of screen sizes from low end phones to the huge surface hub screen. Using relative positioning will be enough for a lot of scenario but as the window size increases, it is always interesting to completely change the layout by moving the controls of page to different locations.  

In this sample, we will use a vertical layout on narrow screens and an horizontal layout on wide screen. On huge wide screens, we will also change the items' sizes.

<StackPanel x:Name="mainPanel" Background="Black">
    <Border x:Name="item1"
            Background="Teal"
            Width="50"
            Height="50">
        <TextBlock Text="Item 1"
                   VerticalAlignment="Center"
                   HorizontalAlignment="Center" />
    </Border>

    <Border x:Name="item2"
            Background="Aquamarine"
            Width="50"
            Height="50">
        <TextBlock Text="Item 2"
                   VerticalAlignment="Center"
                   HorizontalAlignment="Center" />
    </Border>

    <Border x:Name="item3"
            Background="LightCoral"
            Width="50"
            Height="50">
        <TextBlock Text="Item 3"
                   VerticalAlignment="Center"
                   HorizontalAlignment="Center" />
    </Border>

    <VisualStateManager.VisualStateGroups>
        <VisualStateGroup>

            <VisualState x:Name="ultrawide">
                <VisualState.StateTriggers>
                    <AdaptiveTrigger MinWindowWidth="800" />
                </VisualState.StateTriggers>

                <VisualState.Setters>
                    <Setter Target="mainPanel.Orientation" Value="Horizontal" />
                    <Setter Target="item1.Width" Value="100" />
                    <Setter Target="item1.Height" Value="100" />
                    <Setter Target="item2.Width" Value="100" />
                    <Setter Target="item2.Height" Value="100" />
                    <Setter Target="item3.Width" Value="100" />
                    <Setter Target="item3.Height" Value="100" />
                </VisualState.Setters>
            </VisualState>

            <VisualState x:Name="wide">
                <VisualState.StateTriggers>
                    <AdaptiveTrigger MinWindowWidth="600" />
                </VisualState.StateTriggers>

                <VisualState.Setters>
                    <Setter Target="mainPanel.Orientation" Value="Horizontal" />
                </VisualState.Setters>
            </VisualState>

            <VisualState x:Name="narrow" />
        </VisualStateGroup>
    </VisualStateManager.VisualStateGroups>
</StackPanel>

When the window is resized, the system will compare the current window's width with the minimum width from the AdaptiveTrigger. If the current width is greater or equal than the minimum width, the trigger will be activated and the corresponding VisualState being displayed.

Here is the output for the different states

**Narrow**
[![narrow][1]][1]

**Wide**
[![wide][2]][2]

**Ultrawide**
[![ultrawide][3]][3]


  [1]: http://i.stack.imgur.com/oNY0X.png
  [2]: http://i.stack.imgur.com/jww1c.png
  [3]: http://i.stack.imgur.com/BWIVm.png

