---
title: "Triggers"
slug: "triggers"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Discussion on the various types of Triggers available in WPF, including `Trigger`, `DataTrigger`, `MultiTrigger`, `MultiDataTrigger`, and `EventTrigger`.

Triggers allow any class that derives from `FrameworkElement` or `FrameworkContentElement` to set or change their properties based on certain conditions defined in the trigger. Basically, if an element can be styled, it can be triggered as well.

 - All triggers, except for `EventTrigger` must be defined within a `<Style>` element. An `EventTrigger` may be defined in either a `<Style>` element, or a control's `Triggers` property.
 - `<Trigger>` elements may contain any number of `<Setter>` elements. These elements are responsible for setting properties on the containing element when the `<Trigger>` element's condition is met.
 - If a property is defined in the root element markup, the property change defined in the `<Setter>` element will not take effect, even if the trigger condition has been met. Consider the markup `<TextBlock Text="Sample">`. The `Text` property of the proceeding code will never change based on a trigger because root property definitions take precidence over properties defined in styles.
 - Like bindings, once a trigger has been used, it cannot be modified.

## DataTrigger
A `DataTrigger` can be attached to any property, be it on it's own control, another control, or even a property in a non UI class. Consider the following simple class.

    public class Cheese
    {
        public string Name { get; set; }
        public double Age { get; set; }
        public int StinkLevel { get; set; }
    }
Which we will attach as the `DataContext` in the following `TextBlock`.

    <TextBlock Text="{Binding Name}">
        <TextBlock.DataContext>
            <local:Cheese Age="12" StinkLevel="100" Name="Limburger"/>
        </TextBlock.DataContext>
        <TextBlock.Style>
            <Style TargetType="{x:Type TextBlock}">
                <Style.Triggers>
                    <DataTrigger Binding="{Binding StinkLevel}" Value="100">
                        <Setter Property="Foreground" Value="Green"/>
                    </DataTrigger>
                </Style.Triggers>
            </Style>
        </TextBlock.Style>
    </TextBlock>
In the preceeding code, the `TextBlock.Foreground` property will be Green. If we change the `StinkLevel` property in our XAML to anything other than 100, the `Text.Foreground` property will revert to it's default value.



## Trigger
The simplest of the five trigger types, the `Trigger` is responsible for setting properties based on other properties <b>within the same control</b>.

    <TextBlock>
        <TextBlock.Style>
            <Style TargetType="{x:Type TextBlock}">
                <Style.Triggers>
                    <Trigger Property="Text" Value="Pass">
                        <Setter Property="Foreground" Value="Green"/>
                    </Trigger>
                </Style.Triggers>
            </Style>
        </TextBlock.Style>
    </TextBlock>
In this example, the foreground color of the `TextBlock` will turn green when it's `Text` property is equal to the string `"Pass"`.

## MultiTrigger
A `MultiTrigger` is similar to a standard `Trigger` in that it only applies to properties <b>within the same control</b>. The difference is that a `MultiTrigger` has multiple conditions which must be satisfied before the trigger will operate. Conditions are defined using the `<Condition>` tag.

    <TextBlock x:Name="_txtBlock" IsEnabled="False">
        <TextBlock.Style>
            <Style TargetType="{x:Type TextBlock}">
                <Style.Triggers>
                    <MultiTrigger>
                        <MultiTrigger.Conditions>
                            <Condition Property="Text" Value="Pass"/>
                            <Condition Property="IsEnabled" Value="True"/>
                        </MultiTrigger.Conditions>
                        <Setter Property="Foreground" Value="Green"/>
                    </MultiTrigger>
                </Style.Triggers>
            </Style>
        </TextBlock.Style>
    </TextBlock>

Notice the `MultiTrigger` will not activate until both conditions are met.

