---
title: "Styles in WPF"
slug: "styles-in-wpf"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

# Introductory remarks

In WPF, a **Style** defines the values of one or more dependency properties for a given visual element. Styles are used throughout the application to make the user interface more consistent (e.g. giving all dialog buttons a consistent size) and to make bulk changes easier (e.g. changing the width of all buttons.)

Styles are typically defined in a `ResourceDictionary` at a high level in the application (e.g. in *App.xaml* or in a theme) so it is available app-wide, but they may also be defined for a single element and its children, e.g. applying a style to all `TextBlock` elements inside a `StackPanel`.

    <StackPanel>
        <StackPanel.Resources>
            <Style TargetType="TextBlock">
                <Setter Property="Margin" Value="5,5,5,0"/>
                <Setter Property="Background" Value="#FFF0F0F0"/>
                <Setter Property="Padding" Value="5"/>
            </Style>
        </StackPanel.Resources>
            
        <TextBlock Text="First Child"/>
        <TextBlock Text="Second Child"/>
        <TextBlock Text="Third Child"/>      
    </StackPanel>

# Important notes
 * The location where the style is defined affects where it is available.
 * Forward references cannot be resolved by `StaticResource`. In other words, if you're defining a style that depends upon another style or resource in a resource dictionary, it must be defined after/below the resource upon which it depends.
 * `StaticResource` is the recommended way to reference styles and other resources (for performance and behavioral reasons) unless you specifically require the use of `DynamicResource`, e.g. for themes that can be changed at runtime.

# Resources

MSDN has thorough articles on styles and resources that have more depth than is possible to provide here.

 - [Resources Overview][1]
 - [Styling and Templating][2]
 - [Control Authoring Overview][3]

  [1]: https://msdn.microsoft.com/en-us/library/ms750613(v=vs.90).aspx
  [2]: https://msdn.microsoft.com/en-us/library/ms745683(v=vs.110).aspx
  [3]: https://msdn.microsoft.com/en-us/library/ms745025(v=vs.110).aspx

## Defining a named style
A named style requires the `x:Key` property to be set and applies only to elements that explicitly reference it by name:

    <StackPanel>
        <StackPanel.Resources>
            <Style x:Key="MyTextBlockStyle" TargetType="TextBlock">
                <Setter Property="Background" Value="Yellow"/>
                <Setter Property="FontWeight" Value="Bold"/>
            </Style>
        </StackPanel.Resources>
            
        <TextBlock Text="Yellow and bold!" Style="{StaticResource MyTextBlockStyle}" />
        <TextBlock Text="Also yellow and bold!" Style="{DynamicResource MyTextBlockStyle}" />
        <TextBlock Text="Plain text." />      
    </StackPanel>

## Defining an implicit style
An implicit style applies to all elements of a given type within scope. An implicit style can omit `x:Key` since it is implicitly the same as the style's `TargetType` property.

    <StackPanel>
        <StackPanel.Resources>
            <Style TargetType="TextBlock">
                <Setter Property="Background" Value="Yellow"/>
                <Setter Property="FontWeight" Value="Bold"/>
            </Style>
        </StackPanel.Resources>

        <TextBlock Text="Yellow and bold!"  />
        <TextBlock Text="Also yellow and bold!" />
        <TextBlock Style="{x:Null}" Text="I'm not yellow or bold; I'm the control's default style!" />
    </StackPanel>

## Inheriting from a style
It is common to need a base style that defines properties/values shared between multiple styles belonging to the same control, especially for something like `TextBlock`. This is accomplished by using the `BasedOn` property. Values are inherited and then can be overridden.

    <Style x:Key="BaseTextBlockStyle" TargetType="TextBlock">
        <Setter Property="FontSize" Value="12"/>
        <Setter Property="Foreground" Value="#FFBBBBBB" />
        <Setter Property="FontFamily" Value="Arial" />
    </Style>

    <Style x:Key="WarningTextBlockStyle"
           TargetType="TextBlock"
           BasedOn="{StaticResource BaseTextBlockStyle">
        <Setter Property="Foreground" Value="Red"/>
        <Setter Property="FontWeight" Value="Bold" />
    </Style>

In the above example, any `TextBlock` using the style `WarningTextBlockStyle` would be presented as 12px Arial in red and bold.

Because implicit styles have an implicit `x:Key` that matches their `TargetType`, you can inherit those as well:

    <!-- Implicit -->
    <Style TargetType="TextBlock">
        <Setter Property="FontSize" Value="12"/>
        <Setter Property="Foreground" Value="#FFBBBBBB" />
        <Setter Property="FontFamily" Value="Arial" />
    </Style>

    <Style x:Key="WarningTextBlockStyle"
           TargetType="TextBlock"
           BasedOn="{StaticResource {x:Type TextBlock}}">
        <Setter Property="Foreground" Value="Red"/>
        <Setter Property="FontWeight" Value="Bold" />
    </Style>

