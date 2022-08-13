---
title: "Introduction to WPF Data Binding"
slug: "introduction-to-wpf-data-binding"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Syntax
- {Binding PropertyName} *is equivalent to* {Binding Path=PropertyName}
- {Binding Path=SomeProperty.SomeOtherProperty.YetAnotherProperty}
- {Binding Path=SomeListProperty[1]}
    

## Parameters
| Parameter | Details |
| ------ | ------ |
| Path   | Specifies the path to bind to. If unspecified, binds to the DataContext itself. |
| UpdateSourceTrigger | Specifies when the binding source has its value updated. Defaults to `LostFocus`. Most used value is `PropertyChanged`.
| Mode | Typically `OneWay` or `TwoWay`. If unspecified by the binding, it defaults to `OneWay` unless the binding target requests it to be `TwoWay`. An error occurs when `TwoWay` is used to bind to a readonly property, e.g. `OneWay` must be explicitly set when binding a readonly string property to `TextBox.Text`.
| Source | Allows for using a `StaticResource` as a binding source instead of the current DataContext.
| RelativeSource | Allows for using another XAML element as a binding source instead of the current DataContext.
| ElementName | Allows for using a named XAML element as a binding source instead of the current DataContext.
| FallbackValue | If the binding fails, this value is provided to the binding target.
| TargetNullValue | If the binding source value is `null`, this value is provided to the binding target.
| Converter | Specifies the converter `StaticResource` that is used to convert the binding's value, e.g. convert a boolean to a `Visibility` enum item.
| ConverterParameter | Specifies an optional parameter to be provided to the converter. This value must be static and cannot be bound.
| StringFormat | Specifies a format string to be used when displaying the bound value.
| Delay | (WPF 4.5+) Specifies a Delay in `milliseconds` for the binding to update the `BindingSource` in the `ViewModel`. This must be used with `Mode=TwoWay` and `UpdateSourceTrigger=PropertyChanged` to take effect.

UpdateSourceTrigger
-------------------

By default, WPF updates the binding source when the control loses focus. However, if there is only one control that can get focus -- something that's common in examples -- you will need to specify `UpdateSourceTrigger=PropertyChanged` for the updates to work.

You will want want to use `PropertyChanged` as the trigger on many two-way bindings unless updating the binding source on every keystroke is costly or live data validation is undesirable.

Using `LostFocus` has an unfortunate side effect: pressing enter to submit a form using a button marked `IsDefault` does not update the property backing your binding, effectively undoing your changes. Fortunately, [some workarounds exist][1].

Please also note that, unlike UWP, WPF (4.5+) also has the `Delay` property in bindings, wich might just be enough for some Bindings with local-only or simple minor intelligence settings, like some `TextBox` validations.

  [1]: https://stackoverflow.com/questions/18840645/enter-press-doesnt-raise-property-changed-event-wpf/ "enter press doesn't raise property changed event wpf"


## Convert a boolean to visibility value
This example hides the red box (border) if the checkbox is not checked by making use of an `IValueConverter`.

**Note:** The `BooleanToVisibilityConverter` used in the example below is a built-in value converter, located in the System.Windows.Controls namespace. 

XAML:

    <Window x:Class="StackOverflowDataBindingExample.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            Title="MainWindow" Height="350" Width="525">
        <Window.Resources>
            <BooleanToVisibilityConverter x:Key="VisibleIfTrueConverter" />
        </Window.Resources>
        <StackPanel>
            <CheckBox x:Name="MyCheckBox"
                      IsChecked="True" />
            <Border Background="Red" Width="20" Height="20"
                    Visibility="{Binding Path=IsChecked,ElementName=MyCheckBox, Converter={StaticResource VisibleIfTrueConverter}}" />
        </StackPanel>
    </Window>


  [1]: https://msdn.microsoft.com/en-us/library/system.windows.controls.booleantovisibilityconverter(v=vs.110).aspx

## Implementing INotifyPropertyChanged
`INotifyPropertyChanged` is an interface used by binding sources (i.e. the DataContext) to let the user interface or other components know that a property has been changed. WPF automatically updates the UI for you when it sees the `PropertyChanged` event raised. It is desirable to have this interface implemented on a base class that all of your viewmodels can inherit from.

In C# 6, this is all you need:

    public abstract class ViewModelBase : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        protected void NotifyPropertyChanged([CallerMemberName] string name = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
        }
    }

This allows you to invoke `NotifyPropertyChanged` in two different ways:

 1. `NotifyPropertyChanged()`, which will raise the event for the setter that invokes it, thanks to the attribute [CallerMemberName][3].
 2. `NotifyPropertyChanged(nameof(SomeOtherProperty))`, which will raise the event for SomeOtherProperty.

For .NET 4.5 and above using C# 5.0, this can be used instead:

    public abstract class ViewModelBase : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        protected void NotifyPropertyChanged([CallerMemberName] string name = null)
        {
            var handler = PropertyChanged;
            if (handler != null)
            {
                handler(this, new PropertyChangedEventArgs(name));
            }
        }
    }

In versions of .NET prior to 4.5, you have to settle for property names as string constants or [a solution using expressions][1].

----
**Note:** It is possible to bind to a property of a "plain old C# object" (POCO) that does not implement `INotifyPropertyChanged` and observe that the bindings work better than expected. This is a hidden feature in .NET and should probably be avoided. Especially as it will cause memory leaks when the binding's `Mode` is not `OneTime` (see [here][4]).

[Why does the binding update without implementing INotifyPropertyChanged?][2]

  [1]: https://stackoverflow.com/questions/671968/retrieving-property-name-from-lambda-expression
  [2]: https://stackoverflow.com/questions/7767218/why-does-the-binding-update-without-implementing-inotifypropertychanged
  [3]: https://msdn.microsoft.com/en-us/library/system.runtime.compilerservices.callermembernameattribute(v=vs.110).aspx
  [4]: https://support.microsoft.com/en-us/kb/938416



## Bind to property of an ancestor
You can bind to a property of an ancestor in the visual tree by using a `RelativeSource` binding. The nearest control higher in the visual tree which has the same type or is derived from the type you specify will be used as the binding's source:

    <Grid Background="Blue">
        <Grid Background="Gray" Margin="10">
            <Border Background="Red" Margin="20">
                <StackPanel Background="White" Margin="20">
                    <Button Margin="10" Content="Button1" Background="{Binding Background, RelativeSource={RelativeSource Mode=FindAncestor, AncestorType={x:Type Grid}}}" />
                    <Button Margin="10" Content="Button2" Background="{Binding Background, RelativeSource={RelativeSource Mode=FindAncestor, AncestorType={x:Type FrameworkElement}}}" />
                </StackPanel>
            </Border>
        </Grid>
    </Grid>

In this example, *Button1* has a gray background because the closest `Grid` ancestor has a gray background. *Button2* has a white background because the closest ancestor derived from `FrameworkElement` is the white `StackPanel`.

[![RelativeSource binding example][1]][1]

  [1]: http://i.stack.imgur.com/C7MI0.png

## Bind to property of another named element
You can bind to a property on a named element, but the named element must be in scope.

    <StackPanel>
        <CheckBox x:Name="MyCheckBox" IsChecked="True" />
        <TextBlock Text="{Binding IsChecked, ElementName=MyCheckBox}" />
    </StackPanel>

## Defining the DataContext
In order to work with bindings in WPF, you need to define a **DataContext**. The DataContext tells bindings where to get their data from by default. 

    <Window x:Class="StackOverflowDataBindingExample.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
            xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
            xmlns:local="clr-namespace:StackOverflowDataBindingExample"
            xmlns:vm="clr-namespace:StackOverflowDataBindingExample.ViewModels"
            mc:Ignorable="d"
            Title="MainWindow" Height="350" Width="525">
        <Window.DataContext>
            <vm:HelloWorldViewModel />
        </Window.DataContext>
        ...
    </Window>

You can also set the DataContext through code-behind, but it is worth noting that XAML IntelliSense is somewhat picky: a strongly-typed DataContext must be set in XAML for IntelliSense to suggest properties available for binding.

    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            DataContext = new HelloWorldViewModel();
        }
    }

While there are frameworks to help you define your DataContext in a more flexible way (e.g. [MVVM Light][1] has a viewmodel locator that uses [inversion of control][2]), we use the quick and dirty method for the purposes of this tutorial.

You can define a DataContext for pretty much any visual element in WPF. The DataContext is generally inherited from ancestors in the visual tree unless it has been explicitly overridden, e.g. inside a ContentPresenter.

  [1]: https://mvvmlight.codeplex.com/ "MVVM Light project page"
  [2]: https://stackoverflow.com/questions/3058/what-is-inversion-of-control "What is inversion of control?"

## Binding multiple values with a MultiBinding
The MultiBinding allows binding multiple values to the same property.
In the following example multiple values are bound to the Text property of a Textbox and formatted using the StringFormat property.

    <TextBlock>
        <TextBlock.Text>
            <MultiBinding StringFormat="{}{0} {1}">
                <Binding Path="User.Forename"/>
                <Binding Path="User.Surname"/>
            </MultiBinding>
        </TextBlock.Text>
    </TextBlock>

Apart from `StringFormat`, an `IMultiValueConverter`could also be used to convert the values from the Bindings to one value for the MultiBinding's target.

However, MultiBindings cannot be nested.

