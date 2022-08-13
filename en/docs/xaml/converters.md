---
title: "Converters"
slug: "converters"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Parameters
| Parameter | Details|
| ------ | ------ |
| value | The value to convert from   |
| targetType | The type being converted to |
| parameter | Optional value to control how the conversion works |
| culture | CultureInfo object - required if localisation needed |

The `Convert` method converts the value from the source (usually the view model) to the target (usually a property of a control).

The `ConvertBack` method converts the value from the target back to the source. It is only needed if the binding is `TwoWay` or `OneWayToSource`.

When a `ConvertBack` is not supported, i.e. there is no one-to-one mapping between the pre-conversion value and the post-conversion value, it's common practice to have the `ConvertBack` method return `DependencyProperty.UnsetValue`. It's a better option than throwing an exception (e.g. `NotImplementedException`) as it avoids unexpected runtime errors. Also, bindings can benefit of their `FallbackValue` when `DependencyProperty.UnsetValue` is returned by a converter.

## Converters 101
XAML controls may have dependency properties that can be bound to objects from `DataContext` or other controls. When the type of the object being bound is different from the type of the target `DependencyProperty`, a **converter** may be used to adapt one type to another.

Converters are classes implementing `System.Windows.Data.IValueConverter` or `System.Windows.Data.IMultiValueConverter`; WPF implements some out of the box converters, but developers may see use in custom implementations, as it is frequently the case.

To use a converter in XAML, an instance can be instantiated in the `Resources` section. For the example below, `System.Windows.Controls.BooleanToVisibilityConverter` will be used:

    <UserControl.Resources>
        <BooleanToVisibilityConverter x:Key="BooleanToVisibilityConverter"/>
    </UserControl.Resources>

Notice the `x:Key` element defined, which is then used to reference the instance of `BooleanToVisibilityConverter` in the binding:

    <TextBlock Text="This will be hidden if property 'IsVisible' is true"
               Visibility="{Binding IsVisible, 
                                    Converter={StaticResource BooleanToVisibilityConverter}}"/>

In the example above, a boolean `IsVisible` property is converted to a value of the `System.Windows.Visibility` enumeration; `Visibility.Visible` if true, or `Visibility.Collapsed` otherwise.

## String to IsChecked Converter
In XAML:

    <RadioButton IsChecked="{Binding EntityValue, Mode=TwoWay,
                             Converter={StaticResource StringToIsCheckedConverter},
                             ConverterParameter=Male}"
                 Content="Male"/>
    
    <RadioButton IsChecked="{Binding EntityValue, Mode=TwoWay,
                             Converter={StaticResource StringToIsCheckedConverter},
                             ConverterParameter=Female}"
                 Content="Female"/>

The C# class:

    public class StringToIsCheckedConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
        {
            string input = (string)value;
            string test = (string)parameter;
            return input == test;
        }
    
        public object ConvertBack(object value, Type targetType, object parameter, System.Globalization.CultureInfo culture)
        {
            if (value == null || !(value is bool))
            {
                return string.Empty;
            }
            if (parameter == null || !(parameter is string))
            {
                return string.Empty;
            }
            if ((bool)value)
            {
                return parameter.ToString();
            }
            else
            {
                return string.Empty;
            }
        }
    }


## Creating and using a Converter: BooleanToVisibilityConverter and InvertibleBooleanToVisibilityConverter
To extend and expand upon the binding experience we have converters to convert one a value of one type into another value of another type.
To leverage Converters in a Databinding you first need to create a DataConverter class tht extens either <br/>

 - `IValueConverter`**(WPF & UWP)** </p>

or <br/>
 - `IMultiValueConverter`**(WPF)**<br/>

if you want to convert multiple types into one type<br/>In this case we focus on converting a `boolean True/False` value to the correspionding Visibilities `Visibility.Visible` and `Visibility.Collapsed`:
<!-- language: lang-cs -->
    public class BooleanToVisibilityConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, string language)
        {
            return (value is bool && (bool) value) ? Visibility.Visible : Visibility.Collapsed;
        }
    
        public object ConvertBack(object value, Type targetType, object parameter, string language)
        {
            return (value is Visibility && (Visibility) value == Visibility.Visible);
        }
    }
The **`Convert`** method is called whenever you **`GET`** data **`FROM`** the **`ViewModel`**.<br/>
The **`ConvertBack`** is called upon **`SET`** ing data **`TO`** the **`ViewModel`** for **`BindingMode.TwoWay`** bindings.</p>
Of course  you can also utilize properties within your converter. Take a look at this one:<br/>
<!-- language: lang-cs -->
    public class InvertibleBooleanToVisibilityConverter : IValueConverter
    {
        public bool Invert { get; set; } = false;
    
        public object Convert(object value, Type targetType, object parameter, string language)
        {
            return (value is bool && (bool) value != Invert) ? Visibility.Visible : Visibility.Collapsed;
        }
    
        public object ConvertBack(object value, Type targetType, object parameter, string language)
        {
            return (value is Visibility && ((Visibility) value == Visibility.Visible) != Invert);
        }
    }

If you want to use a converter in a `Binding`, simply declare it as a resource in your page, window, or other element, give it a key and supply potentially needed properties:
<!-- language: lang-xml -->
    <Page ...
        xmlns:converters="using:MyNamespce.Converters">
    <Page.Resources>
        <converters:InvertibleBooleanToVisibilityConverter 
            x:Key="BooleanToVisibilityConverter" 
            Invert="False" />
    </Page.Resources>

and use it as a `StaticResource` in a binding:
<!-- language: lang-xml -->
    <ProgressRing 
            Visibility="{Binding ShowBusyIndicator, 
                Converter={StaticResource BooleanToVisibilityConverter},
                UpdateSourceTrigger=PropertyChanged,
                Mode=OneWay}" />

