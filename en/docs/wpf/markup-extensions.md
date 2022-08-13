---
title: "Markup Extensions"
slug: "markup-extensions"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Parameters
| Method | Description |
| ------ | ------ |
| ProvideValue | MarkupExtension class has only one method that should be overridden, XAML parser then uses the value provided by this method to evaluate the result of markup extension. |

A markup extension can be implemented to provide values for properties in an attribute usage, properties in a property element usage, or both.

When used to provide an attribute value, the syntax that distinguishes a markup extension sequence to a XAML processor is the presence of the opening and closing curly braces ({ and }). The type of markup extension is then identified by the string token immediately following the opening curly brace.

When used in property element syntax, a markup extension is visually the same as any other element used to provide a property element value: a XAML element declaration that references the markup extension class as an element, enclosed within angle brackets (<>).

For more info visit https://msdn.microsoft.com/en-us/library/ms747254(v=vs.110).aspx

## Markup Extension used with IValueConverter
One of the best uses for markup extensions is for easier usage of IValueConverter. In the sample below BoolToVisibilityConverter is a value converter but since it's instance independent it can be used without the normal hasles of a value converter with the help of markup extension.
In XAML just use 

    Visibility="{Binding [BoolProperty], Converter={[xmlns]:BoolToVisibilityConverter}}"

and you can set item visibility to bool value.

    public class BoolToVisibilityConverter : MarkupExtension, IValueConverter
        {
            public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
            {
                if (value is bool)
                    return (bool)value ? Visibility.Visible : Visibility.Collapsed;
                else
                    return Visibility.Collapsed;
            }
    
            public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            {
                if (value is Visibility)
                {
                    if ((Visibility)value == Visibility.Visible)
                        return true;
                    else
                        return false;
                }
                else
                    return false;
            }
    
            public override object ProvideValue(IServiceProvider serviceProvider)
            {
                return this;
            }
        }

## XAML-Defined markup extensions
There are four predefined markup extensions in XAML:

`x:Type` supplies the Type object for the named type. This facility is used most frequently in styles and templates.

    <object property="{x:Type prefix:typeNameValue}" .../>

`x:Static` produces static values. The values come from value-type code entities that are not directly the type of a target property's value, but can be evaluated to that type.

    <object property="{x:Static prefix:typeName.staticMemberName}" .../>

`x:Null` specifies null as a value for a property and can be used either for attributes or property element values.

    <object property="{x:Null}" .../>

`x:Array` provides support for the creation of general arrays in XAML syntax, for cases where the collection support provided by WPF base elements and control models is deliberately not used.

    <x:Array Type="typeName">
      arrayContents
    </x:Array>



