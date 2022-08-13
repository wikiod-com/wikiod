---
title: "Binding vs xBind"
slug: "binding-vs-xbind"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Refer the official [Data binding documentation][1] from Microsoft.


  [1]: https://docs.microsoft.com/en-us/windows/uwp/data-binding/data-binding-quickstart

## When to use x:Bind
 - When calling methods directly from the view.
 - If performance matters really bad (scientific spaceship stuff)
 - When you want to get compile time errors

## Binding modes and defaults
There are three modes of XAML bindings exists for either `Binding` and `x:Bind`:
 - **OneTime**: Update happens only once, on initialization of the view during `InitializeComponent()` call. *(ViewModel[sends data when initializing] -> View)*
 - **OneWay**: View is updated when ViewModel changes. But not in the reverse direction. *(ViewModel -> View)*
 - **TwoWay**: View is updated when ViewModel changes and vice versa. *(ViewModel <-> View)*

Default mode of `Binding` is `OneWay` and that of `x:Bind` is `OneTime`.


Select the modes like this:

    <TextBlock Text="{Binding SomeText, Mode=TwoWay}" /> <!-- Binding -->
    <TextBlock Text="{x:Bind SomeText, Mode=OneWay}" /> <!-- x:Bind -->

## When to use Binding
 - Use it if you want to be flexible about the source type of your data. It won't bind to an actual property but to its name.
 - If you want to bind to the DataContext

