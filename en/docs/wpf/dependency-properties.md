---
title: "Dependency Properties"
slug: "dependency-properties"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Dependency Properties are a type of property that extend out a CLR property.  Whereas a CLR property is read directly from a member of your class, a Dependency Property will be dynamically resolved when calling the GetValue() method that your object gains via inheritance from the base DependencyObject class.

This section will break down Dependency Properties and explain their usage both conceptually and through code examples.

## Syntax
- DependencyProperty.Register(string name, Type propertyType, Type ownerType)
- DependencyProperty.Register(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata)
- DependencyProperty.Register(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata, ValidateValueCallback validateValueCallback)
- DependencyProperty.RegisterAttached(string name, Type propertyType, Type ownerType)
- DependencyProperty.RegisterAttached(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata)
- DependencyProperty.RegisterAttached(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata, ValidateValueCallback validateValueCallback)
- DependencyProperty.RegisterReadOnly(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata)
- DependencyProperty.RegisterReadOnly(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata, ValidateValueCallback validateValueCallback)
- DependencyProperty.RegisterAttachedReadOnly(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata)
- DependencyProperty.RegisterAttachedReadOnly(string name, Type propertyType, Type ownerType, PropertyMetadata typeMetadata, ValidateValueCallback validateValueCallback)

## Parameters
| Parameter | Details |
| ------ | ------ |
| name | The `String` representation of the property's name |
| propertyType | The `Type` of the property, e.g. `typeof(int)` |
| ownerType | The `Type` of the class in which the property is being defined, e.g. `typeof(MyControl)` or `typeof(MyAttachedProperties)`. |
| typeMetadata | Instance of `System.Windows.PropertyMetadata` (or one of its subclasses) that defines default values, property changed callbacks, `FrameworkPropertyMetadata` allows for defining binding options like `System.Windows.Data.BindingMode.TwoWay`.
| validateValueCallback | Custom callback that returns true if the property's new value is valid, otherwise false.

## Standard dependency properties
# When to use
Virtually all WPF controls make heavy use of dependency properties. A dependency property allows for the use of many WPF features that are not possible with standard CLR properties alone, including but not limited to support for styles, animations, data binding, value inheritance, and change notifications.

The `TextBox.Text` property is a simple example of where a standard dependency property is needed. Here, data binding wouldn't be possible if `Text` was a standard CLR property.

    <TextBox Text="{Binding FirstName}" />

# How to define
Dependency properties can only be defined in classes derived from `DependencyObject`, such as `FrameworkElement`, `Control`, etc.

One of the fastest ways to create a standard dependency property without having to remember the syntax is to use the "propdp" snippet by typing `propdp` and then pressing <kbd>Tab</kbd>. A code snippet will be inserted that can then be modified to suit your needs:

    public class MyControl : Control
    {
        public int MyProperty
        {
            get { return (int)GetValue(MyPropertyProperty); }
            set { SetValue(MyPropertyProperty, value); }
        }
    
        // Using a DependencyProperty as the backing store for MyProperty.
        // This enables animation, styling, binding, etc...
        public static readonly DependencyProperty MyPropertyProperty =
            DependencyProperty.Register("MyProperty", typeof(int), typeof(MyControl),
                new PropertyMetadata(0));
    }

You should <kbd>Tab</kbd> through the different parts of the code snippet to make the necessary changes, including updating the property name, property type, containing class type, and the default value.

# Important conventions
There are a few important conventions/rules to follow here:

 1. **Create a CLR property for the dependency property.** This property is used in your object's code-behind or by other consumers. It should invoke `GetValue` and `SetValue` so consumers don't have to.

 2. **Name the dependency property correctly.** The `DependencyProperty` field should be `public static readonly`. It should have a name that corresponds with the CLR property name and ends with "Property", e.g. `Text` and `TextProperty`.

 3. **Do not add additional logic to CLR property's setter.** The dependency property system (and XAML specifically) does not make use of the CLR property. If you want to perform an action when the property's value changes, you must provide a callback via `PropertyMetadata`:

        public static readonly DependencyProperty MyPropertyProperty =
            DependencyProperty.Register("MyProperty", typeof(int), typeof(MyControl),
                new PropertyMetadata(0, MyPropertyChangedHandler));
    
        private static void MyPropertyChangedHandler(DependencyObject sender, DependencyPropertyChangedEventArgs args)
        {
            // Use args.OldValue and args.NewValue here as needed.
            // sender is the object whose property changed.
            // Some unboxing required.
        }

# Binding mode

To eliminate the need for specifying `Mode=TwoWay` in bindings (akin to the behavior of `TextBox.Text`) update the code to use `FrameworkPropertyMetadata` instead of `PropertyMetadata` and specify the appropriate flag:

    public static readonly DependencyProperty MyPropertyProperty =
        DependencyProperty.Register("MyProperty", typeof(int), typeof(MyControl), 
            new FrameworkPropertyMetadata(0, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

## Attached dependency properties
# When to use
An attached property is a dependency property that can be applied to any `DependencyObject` to enhance the behavior of various controls or services that are aware of the property's existence.

Some use cases for attached properties include:

 1. Having a parent element iterate through its children and act upon the children in a certain way. For example, the `Grid` control uses the `Grid.Row`, `Grid.Column`, `Grid.RowSpan`, and `Grid.ColumnSpan` attached properties to arrange elements into rows and columns.
 2. Adding visuals to existing controls with custom templates, e.g adding watermarks to empty text boxes app-wide without having to subclass `TextBox`.
 3. Providing a generic service or feature to some or all existing controls, e.g. `ToolTipService` or `FocusManager`. These are commonly referred to as *attached behaviors*.
 4. When inheritance down the visual tree is required, e.g. similar to the behavior of `DataContext`.

This further demonstrates what is happening in the `Grid` use case:

    <Grid>
        <Grid.ColumnDefinitions>
            <ColumnDefinition />
            <ColumnDefinition />
        </Grid.ColumnDefinitions>
    
        <Label Grid.Column="0" Content="Your Name:" />
        <TextBox Grid.Column="1" Text="{Binding FirstName}" />
    </Grid>

`Grid.Column` is not a property that exists on either `Label` or `TextBox`. Rather, the `Grid` control looks through its child elements and arranges them according to the values of the attached properties.

# How to define
We'll continue to use `Grid` for this example. The definition of `Grid.Column` is shown below, but the `DependencyPropertyChangedEventHandler` is excluded for brevity.

    public static readonly DependencyProperty RowProperty =
        DependencyProperty.RegisterAttached("Row", typeof(int), typeof(Grid),
            new FrameworkPropertyMetadata(0, ...));

    public static void SetRow(UIElement element, int value)
    {
        if (element == null)
            throw new ArgumentNullException("element");
 
        element.SetValue(RowProperty, value);
    }
 
    public static int GetRow(UIElement element)
    {
        if (element == null)
            throw new ArgumentNullException("element");
 
        return ((int)element.GetValue(RowProperty));
    }

Because the attached properties can be attached to a wide variety of items, they cannot be implemented as CLR properties. A pair of static methods is introduced instead.

Hence, in contrast to standard dependency properties, attached properties can also be defined in classes that are not derived from `DependencyObject`.

The same naming conventions that apply to regular dependency properties also apply here: the dependency property `RowProperty` has the corresponding methods `GetRow` and `SetRow`.

# Caveats

As [documented on MSDN][1]:

> Although property value inheritance might appear to work for nonattached dependency properties, the inheritance behavior for a nonattached property through certain element boundaries in the run-time tree is undefined. Always use RegisterAttached to register properties where you specify Inherits in the metadata.


  [1]: https://msdn.microsoft.com/en-us/library/ms753197(v=vs.100).aspx "Property Value Inheritance"

## Read-only dependency properties
# When to use
A read-only dependency property is similar to a normal dependency property, but it is structured to not allow having its value set from outside the control. This works well if you have a property that is purely informational for consumers, e.g. `IsMouseOver` or `IsKeyboardFocusWithin`.

# How to define
Just like standard dependency properties, a read-only dependency property must be defined on a class that derives from `DependencyObject`.

    public class MyControl : Control
    {
        private static readonly DependencyPropertyKey MyPropertyPropertyKey = 
            DependencyProperty.RegisterReadOnly("MyProperty", typeof(int), typeof(MyControl),
                new FrameworkPropertyMetadata(0));
    
        public static readonly DependencyProperty MyPropertyProperty = MyPropertyPropertyKey.DependencyProperty;
    
        public int MyProperty
        {
            get { return (int)GetValue(MyPropertyProperty); }
            private set { SetValue(MyPropertyPropertyKey, value); }
        }
    }

The same conventions that apply to regular dependency properties also apply here, but with two key differences:

 1. The `DependencyProperty` is sourced from a `private` `DependencyPropertyKey`.
 2. The CLR property setter is `protected` or `private` instead of `public`.

Note that the setter passes `MyPropertyPropertyKey` and not `MyPropertyProperty` to the `SetValue` method. Because the property was defined read-only, any attempt to use `SetValue` on the property must be used with overload that receives `DependencyPropertyKey`; otherwise, an `InvalidOperationException` will be thrown.

