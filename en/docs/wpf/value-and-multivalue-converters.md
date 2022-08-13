---
title: "Value and Multivalue Converters"
slug: "value-and-multivalue-converters"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

## Parameters
 | Parameter | Details |
 | --------- | ------ |
 | value | The value produced by the binding source. |
 | values | The values array, produced by the binding source. |
 | targetType | The type of the binding target property.|
 | parameter | The converter parameter to use. |
 | culture | The culture to use in the converter. |

## **What IValueConverter and IMultiValueConverterthey are**

IValueConverter and IMultiValueConverter - interfaces that provides a way to apply a custom logic to a binding.

## **What they are useful for**

1. You have a some type value but you want to show zero values in one way and positive numbers in another way
2. You have a some type value and want to show element in one case and hide in another
3. You have a numeric value of money but want to show it as words
4. You have a numeric value but want to show different images for defferent numbers

These are some of the simple cases, but there are many more.

For cases like this, you can use a value converter. 
These small classes, which implement the IValueConverter interface or IMultiValueConverter, will act like middlemen and translate a value between the source and the destination. So, in any situation where you need to transform a value before it reaches its destination or back to its source again, you likely need a converter.



## Group multiple converters [IValueConverter]
This converter will chain multiple converters together.

    public class ValueConverterGroup : List<IValueConverter>, IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return this.Aggregate(value, (current, converter) => converter.Convert(current, targetType, parameter, culture));
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

In this example, the boolean result from `EnumToBooleanConverter` is used as input in `BooleanToVisibilityConverter`.

    <local:ValueConverterGroup x:Key="EnumToVisibilityConverter">
        <local:EnumToBooleanConverter/>
        <local:BooleanToVisibilityConverter/>
    </local:ValueConverterGroup>

The button will only be visible when the `CurrentMode` property is set to `Ready`.

    <Button Content="Ok" Visibility="{Binding Path=CurrentMode, Converter={StaticResource EnumToVisibilityConverter}, ConverterParameter={x:Static local:Mode.Ready}"/>

## Converter with property [IValueConverter]
Show how to create simple converter with parameter via property and then pass it in declaration.
Convert `bool` value to `Visibility`. Allow invert result value by setting `Inverted` property to `True`.

    public class BooleanToVisibilityConverter : IValueConverter
    {
        public bool Inverted { get; set; }

        /// <summary>
        /// Convert bool or Nullable bool to Visibility
        /// </summary>
        /// <param name="value">bool or Nullable bool</param>
        /// <param name="targetType">Visibility</param>
        /// <param name="parameter">null</param>
        /// <param name="culture">null</param>
        /// <returns>Visible or Collapsed</returns>
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            bool bValue = false;
            if (value is bool)
            {
                bValue = (bool)value;
            }
            else if (value is Nullable<bool>)
            {
                Nullable<bool> tmp = (Nullable<bool>)value;
                bValue = tmp ?? false;
            }

            if (Inverted)
                bValue = !bValue;
            return (bValue) ? Visibility.Visible : Visibility.Collapsed;
        }

        /// <summary>
        /// Convert Visibility to boolean
        /// </summary>
        /// <param name="value"></param>
        /// <param name="targetType"></param>
        /// <param name="parameter"></param>
        /// <param name="culture"></param>
        /// <returns>True or False</returns>
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is Visibility)
            {
                return ((Visibility) value == Visibility.Visible) && !Inverted;
            }

            return false;
        }
    }

## **Using the converter**

1) Define namespace

`xmlns:converters="clr-namespace:MyProject.Converters;assembly=MyProject"`

2) Define Resource


    <converters:BooleanToVisibilityConverter x:Key="BoolToVisibilityInvertedConverter"
                                             Inverted="False"/>
3) Use it in binding


    <Button Visibility="{Binding AllowEditing, Converter={StaticResource BoolToVisibilityConverter}}"/>

## Simple add converter [IMultiValueConverter]
Show how to create simple `IMultiValueConverter` converter and use `MultiBinding` in xaml. Get summ of all values passed by `values` array.    

    public class AddConverter : IMultiValueConverter
    {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            decimal sum = 0M;

            foreach (string value in values)
            {
                decimal parseResult;
                if (decimal.TryParse(value, out parseResult))
                {
                    sum += parseResult;
                }
            }

            return sum.ToString(culture);
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }

## **Using the converter**

1) Define namespace


    xmlns:converters="clr-namespace:MyProject.Converters;assembly=MyProject"

2) Define Resource


    <converters:AddConverter x:Key="AddConverter"/>

3) Use it in binding


    
    <StackPanel Orientation="Vertical">
        <TextBox x:Name="TextBox" />
        <TextBox x:Name="TextBox1" />
        <TextBlock >
            <TextBlock.Text>
                <MultiBinding Converter="{StaticResource AddConverter}">
                    <Binding Path="Text" ElementName="TextBox"/>
                    <Binding Path="Text" ElementName="TextBox1"/>
                </MultiBinding>
             </TextBlock.Text>
        </TextBlock>
    </StackPanel>


## Build-In BooleanToVisibilityConverter [IValueConverter]
Converter between boolean and visibility.
Get `bool` value on input and returns `Visibility` value.

**NOTE: This converter have already exists in `System.Windows.Controls` namespace.**

    public sealed class BooleanToVisibilityConverter : IValueConverter
    {
        /// <summary>
        /// Convert bool or Nullable bool to Visibility
        /// </summary>
        /// <param name="value">bool or Nullable bool</param>
        /// <param name="targetType">Visibility</param>
        /// <param name="parameter">null</param>
        /// <param name="culture">null</param>
        /// <returns>Visible or Collapsed</returns>
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            bool bValue = false;
            if (value is bool)
            {
                bValue = (bool)value;
            }
            else if (value is Nullable<bool>)
            {
                Nullable<bool> tmp = (Nullable<bool>)value;
                bValue = tmp.HasValue ? tmp.Value : false;
            }
            return (bValue) ? Visibility.Visible : Visibility.Collapsed;
        }
 
        /// <summary>
        /// Convert Visibility to boolean
        /// </summary>
        /// <param name="value"></param>
        /// <param name="targetType"></param>
        /// <param name="parameter"></param>
        /// <param name="culture"></param>
        /// <returns></returns>
        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value is Visibility)
            {
                return (Visibility)value == Visibility.Visible;
            }
            else
            {
                return false;
            }
        }
    }

## **Using the converter**

1) Define Resource


    <BooleanToVisibilityConverter x:Key="BooleanToVisibilityConverter"/>
3) Use it in binding


    <Button Visibility="{Binding AllowEditing, 
                                 Converter={StaticResource BooleanToVisibilityConverter}}"/>

## Usage converters with ConverterParameter
Show how to create simple converter and use `ConverterParameter` to pass parameter to converter. Multiply value by coefficient passed in ConverterParameter.

    public class MultiplyConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value == null)
                return 0;

            if (parameter == null)
                parameter = 1;

            double number;
            double coefficient;

            if (double.TryParse(value.ToString(), out number) && double.TryParse(parameter.ToString(), out coefficient))
            {
                return number * coefficient;
            }

            return 0;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotSupportedException();
        }
    }


## **Using the converter**

1) Define namespace


    xmlns:converters="clr-namespace:MyProject.Converters;assembly=MyProject"

2) Define Resource


    <converters:MultiplyConverter x:Key="MultiplyConverter"/>

3) Use it in binding


    
    <StackPanel Orientation="Vertical">
        <TextBox x:Name="TextBox" />
        <TextBlock Text="{Binding Path=Text, 
                                  ElementName=TextBox, 
                                  Converter={StaticResource MultiplyConverter},
                                  ConverterParameter=10}"/>
    </StackPanel>


## Using MarkupExtension with Converters to skip recource declaration
Usually to use the converter, we have to define it as resource in the following way:

    <converters:SomeConverter x:Key="SomeConverter"/>

It is possible to skip this step by defining a converter as `MarkupExtension` and implementing the method `ProvideValue`. The following example converts a value to its negative:

    namespace MyProject.Converters
    {
    public class Converter_Negative : MarkupExtension, IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return this.ReturnNegative(value);
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return this.ReturnNegative(value);
        }

        private object ReturnNegative(object value)
        {
            object result = null;
            var @switch = new Dictionary<Type, Action> {
                { typeof(bool), () => result=!(bool)value },
                { typeof(byte), () => result=-1*(byte)value },
                { typeof(short), () => result=-1*(short)value },
                { typeof(int), () => result=-1*(int)value },
                { typeof(long), () => result=-1*(long)value },
                { typeof(float), () => result=-1f*(float)value },
                { typeof(double), () => result=-1d*(double)value },
                { typeof(decimal), () => result=-1m*(decimal)value }
            };

            @switch[value.GetType()]();
            if (result == null) throw new NotImplementedException();
            return result;
        }

        public Converter_Negative()
            : base()
        {
        }

        private static Converter_Negative _converter = null;

        public override object ProvideValue(IServiceProvider serviceProvider)
        {
            if (_converter == null) _converter = new Converter_Negative();
            return _converter;
        }
    }
    }

**Using the converter:**


1. Define namespace

     >xmlns:converters="clr-namespace:MyProject.Converters;assembly=MyProject"

2. Example use of this converter in binding

       <RichTextBox IsReadOnly="{Binding Path=IsChecked, ElementName=toggleIsEnabled, Converter={converters:Converter_Negative}}"/>




## Use IMultiValueConverter to pass multiple parameters to a Command
It is possible to pass multiple bound values as a `CommandParameter` using `MultiBinding` with a very simple `IMultiValueConverter`:

    namespace MyProject.Converters
    {
        public class Converter_MultipleCommandParameters : MarkupExtension, IMultiValueConverter
        {
            public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
            {
                return values.ToArray();
            }
            public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
            {
                throw new NotSupportedException();
            }
    
            private static Converter_MultipleCommandParameters _converter = null;
    
            public override object ProvideValue(IServiceProvider serviceProvider)
            {
                if (_converter == null) _converter = new Converter_MultipleCommandParameters();
                return _converter;
            }
    
            public Converter_MultipleCommandParameters()
                : base()
            {
            }
        }
    }
**Using the converter:**
1. Example implementation - method called when `SomeCommand` is executed (*note: `DelegateCommand` is an implementation of `ICommand` that is not provided in this example*):

        private ICommand _SomeCommand;
        public ICommand SomeCommand
        {
            get { return _SomeCommand ?? (_SomeCommand = new DelegateCommand(a => OnSomeCommand(a))); }
        }

        private void OnSomeCommand(object item)
        {
            object[] parameters = item as object[];

            MessageBox.Show(
                string.Format("Execute command: {0}\nParameter 1: {1}\nParamter 2: {2}\nParamter 3: {3}",
                "SomeCommand", parameters[0], parameters[1], parameters[2]));
        }
2. Define namespace
> xmlns:converters="clr-namespace:MyProject.Converters;assembly=MyProject"
3. Example use of this converter in binding

       <Button Width="150" Height="23" Content="Execute some command" Name="btnTestSomeCommand"
            Command="{Binding Path=SomeCommand}" >
            <Button.CommandParameter>
                <MultiBinding Converter="{converters:Converter_MultipleCommandParameters}">
                    <Binding RelativeSource="{RelativeSource Self}" Path="IsFocused"/>
                    <Binding RelativeSource="{RelativeSource Self}" Path="Name"/>
                    <Binding RelativeSource="{RelativeSource Self}" Path="ActualWidth"/>
                </MultiBinding>
            </Button.CommandParameter>
        </Button>


