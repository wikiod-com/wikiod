---
title: "Creating custom UserControls with data binding"
slug: "creating-custom-usercontrols-with-data-binding"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Note that a UserControl is very different from a Control. One of the primary differences is that a UserControl makes use of a XAML layout file to determine where to place several individual Controls. A Control, on the other hand, is just pure code - there's no layout file at all. In some ways, creating a custom Control can be more effective than creating a custom UserControl. 

## ComboBox with custom default text
This custom UserControl will appear as a regular combobox, but unlike the built-in ComboBox object, it can show the user a default string of text if they have not made a selection yet. 

In order to accomplish this, our UserControl will be made up of two Controls. Obviously we need an actual ComboBox, but we will also use a regular Label to show the default text. 

---

**CustomComboBox.xaml**

    <UserControl x:Class="UserControlDemo.CustomComboBox"
                 xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
                 xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
                 xmlns:cnvrt="clr-namespace:UserControlDemo"
                 x:Name="customComboBox">
        <UserControl.Resources>
            <cnvrt:InverseNullVisibilityConverter x:Key="invNullVisibleConverter" />
        </UserControl.Resources>
        <Grid>
            <ComboBox x:Name="comboBox"
                      ItemsSource="{Binding ElementName=customComboBox, Path=MyItemsSource}"
                      SelectedItem="{Binding ElementName=customComboBox, Path=MySelectedItem}"
                      HorizontalContentAlignment="Left" VerticalContentAlignment="Center"/>
            
            <Label HorizontalAlignment="Left" VerticalAlignment="Center"
                   Margin="0,2,20,2" IsHitTestVisible="False"
                   Content="{Binding ElementName=customComboBox, Path=DefaultText}"
                   Visibility="{Binding ElementName=comboBox, Path=SelectedItem, Converter={StaticResource invNullVisibleConverter}}"/>
        </Grid>
    </UserControl>

As you can see, this single UserControl is actually group of two individual Controls. This allows us some flexibility that is not available in a single ComboBox alone. 

Here are several important things to note: 

 - The UserControl itself has an `x:Name` set. This is because we want to bind to properties that are located in the code-behind, which means it needs some way to reference itself.  
 - Each of the binding on the ComboBox have the UserControl's name as the `ElementName`. This is so that the UserControl knows to look at itself to locate bindings. 
 - The Label is not hit-test visible. This is to give the user the illusion that the Label is part of the ComboBox. By setting `IsHitTestVisible=false`, we disallow the user from hovering over or clicking on the Label - all input is passed through it to the ComboBox below. 
 - The Label uses an `InverseNullVisibility` converter to determine whether it should show itself or not. You can find the code for this at the bottom of this example. 

**CustomComboBox.xaml.cs**

    public partial class CustomComboBox : UserControl
    {
        public CustomComboBox()
        {
            InitializeComponent();
        }

        public static DependencyProperty DefaultTextProperty =
            DependencyProperty.Register("DefaultText", typeof(string), typeof(CustomComboBox));

        public static DependencyProperty MyItemsSourceProperty = 
            DependencyProperty.Register("MyItemsSource", typeof(IEnumerable), typeof(CustomComboBox));

        public static DependencyProperty SelectedItemProperty =
            DependencyProperty.Register("SelectedItem", typeof(object), typeof(CustomComboBox));

        public string DefaultText
        {
            get { return (string)GetValue(DefaultTextProperty); }
            set { SetValue(DefaultTextProperty, value); }
        }

        public IEnumerable MyItemsSource
        {
            get { return (IEnumerable)GetValue(MyItemsSourceProperty); }
            set { SetValue(MyItemsSourceProperty, value); }
        }

        public object MySelectedItem
        {
            get { return GetValue(MySelectedItemProperty); }
            set { SetValue(MySelectedItemProperty, value); }
        }
    }

In the code-behind, we're simply exposing which properties we want to be available to the programmer using this UserControl. Unfortunately, because we don't have direct access to the ComboBox from outside this class, we need to expose duplicate properties (`MyItemsSource` for the ComboBox's `ItemsSource`, for example). However, this is a minor tradeoff considering that we can now use this similarly to a native control. 

---

Here's how the `CustomComboBox` UserControl might be used:

    <Window x:Class="UserControlDemo.UserControlDemo"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:cntrls="clr-namespace:UserControlDemo"
            Title="UserControlDemo" Height="240" Width=200>
        <Grid>
            <cntrls:CustomComboBox HorizontalAlignment="Left" Margin="10,10,0,0" VerticalAlignment="Top" Width="165"
                                   MyItemsSource="{Binding Options}"
                                   MySelectedItem="{Binding SelectedOption, Mode=TwoWay}"
                                   DefaultText="Select an option..."/>
        <Grid>
    </Window>

And the end result: 

[![Figure 1][1]][1] [![enter image description here][2]][2] [![Figure 3][3]][3]

---

Here's the InverseNullVisibilityConverter needed for the Label on the UserControl, which is just a slight variation on [lll's version](http://stackoverflow.com/a/21939778/1828486): 

    public class InverseNullVisibilityConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            return value == null ? Visibility.Visible : Visibility.Hidden;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

  [1]: http://i.stack.imgur.com/11aGj.png
  [2]: http://i.stack.imgur.com/S2WlE.png
  [3]: http://i.stack.imgur.com/DgFM6.png

