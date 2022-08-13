---
title: "Differences in the various XAML dialects"
slug: "differences-in-the-various-xaml-dialects"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

XAML is used in Silverlight, Windows Phone, Windows RT and UWP apps. Sharing code or converting code between these is sometimes harder than desirable due to subtle differences between the various XAML dialects. This topic strives to give an overview of these differences with a short explanation. 


## Compiled data bindings: The {x:Bind} markup extension
Databings are essential for working with XAML. The XAML dialect for UWP apps provides a type of binding: the {x:Bind} markup extension. 

Working with {Binding XXX} and {x:Bind XXX} is mostly equivalent, with the difference that the x:Bind extension works at compile time, which enables better debugging capabilities (e.g. break points) and better performance.
   
    <object property="{x:Bind bindingPath}" />

The x:Bind markup extension is only available for UWP apps. Learn more about this in this MSDN article: https://msdn.microsoft.com/en-us/windows/uwp/data-binding/data-binding-in-depth.


Alternatives for Silverlight, WPF, Windows RT: Use the standard {Binding XXX} syntax:

    <object property="{Binding bindingPath}" />

## Importing namespaces in XAML
Most of the time you need to import namespaces in your XAML file. How this is done is different for the different XAML variants.

For Windows Phone, Silverlight, WPF use the clr-namespace syntax:

    <Window ... xmlns:internal="clr-namespace:rootnamespace.namespace"
                xmlns:external="clr-namespace:rootnamespace.namespace;assembly=externalAssembly"
    >

Windows RT, UWP use the using syntax:

    <Page ... xmlns:internal="using:rootnamespace.namespace"
                xmlns:external="using:rootnamespace.namespace;assembly=externalAssembly"
    >

## Multi Binding
Multi Binding is a feature exclusive for WPF development. It allows a binding to multiple values at once (typically used with a MultiValueConverter).

    <TextBox>
        <TextBox.Text>
            <MultiBinding Converter="{StaticResource MyConverter}">
                <Binding Path="PropertyOne"/>
                <Binding Path="PropertyTwo"/>
            </MultiBinding>
        </TextBox.Text>
    </TextBox>

Platforms other than WPF don't support multi binding. You have to find alternative solutions (like moving the code from view and converters to the viewmodel) or resort 3rd party behaviours like in this article: http://www.damirscorner.com/blog/posts/20160221-MultibindingInUniversalWindowsApps.html)

