---
title: "Data Binding"
slug: "data-binding"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
- `<TextBlock Text="{Binding Title}"/>`

- `<TextBlock Text="{Binding Path=Title}"/>`

- `<TextBlock>
         <TextBlock.Text>
               <Binding Path="Title"/>
         </TextBlock.Text>
    </TextBlock>`

All these tags produce the same result.

## Binding string to Text property
To change UI content in runtime, you can use `Binding`. When binded property is changed from the code, it will be displayed to the UI.

    <TextBlock Text="{Binding Title}"/>

To notify UI about changes, property must raise `PropertyChanged` event from `INotifyPropertyChanged` interface or you can use `Dependency Property`.

The Binding is working if the property "Title" is in the xaml.cs file or in the Datacontext class from the `XAML`.

The Datacontext can be set up in the XAML directly

    <Window x:Class="Application.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:local="clr-namespace:Application">
    <Window.DataContext>
       <local:DataContextClass/>
    </Window.DataContext>



## Formatting String Bindings
When making a bind of something, for example a date you may want to show it in a specific format without messing around with it in the code. 

To do this we can use the StringFormat property.

Here are some examples:

    Text="{Binding Path=ReleaseDate, StringFormat=dddd dd MMMM yyyy}"

This formats my date to the following:

Tuesday 16 August 2016

---------------------------------------------------------------------------------------

Here is another example for temperature.

    Text="{Binding Path=Temp, StringFormat={}{0}°C}"

This formats to:

25°C

## The basics of INotifyPropertyChanged
If you do not only wish to display static objects, but have your UI respond to changes to correlating objects, you need to understand the basics of the `INotifyPropertyChanged` interface.

Assuming we have our `MainWindow`defined as

    <Window x:Class="Application.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
        xmlns:vm="clr-namespace:Application.ViewModels>
        <Window.DataContext>
           <vm:MainWindowViewModel/>
        </Window.DataContext>
        <Grid>
            <TextBlock Text={Binding Path=ApplicationStateText}" />
        </Grid>
    </Window>

With our Viewmodel-Class `MainWindowViewModel` defined as 
<!-- language: lang-cs -->
    namespace Application.ViewModels
    {
        public class MainWindowViewModel
        {
            private string _applicationStateText;
    
            public string ApplicationStateText
            {
                get { return _applicationStateText; }
                set { _applicationStateText = value; }
            }
            public MainWindowViewModel() 
            { 
                ApplicationStateText = "Hello World!";
            }
    
        }
    }
the TextBlock of our Application will display the Text *Hello World* due to its binding. If our ApplicationStateText changes during runtime, our UI will not be notified of such change.<br/>
In order to implement this, our DataSource, in this case our `MainWindowViewModel`, needs to implement the Interface `INotifyPropertyChanged`. This will cause our `Bindings`to be able to subscribe to the `PropertyChangedEvent`. <br/>
All we need to do is to Invoke the `PropertyChangedEventHandler` whenever we change our `ApplicationStateText` Property like this:
<!-- language: lang-cs -->
    using System.ComponentModel;
    using System.Runtime.CompilerServices;

    namespace Application.ViewModels
    {
        public class MainWindowViewModel : INotifyPropertyChanged
        {
            public event PropertyChangedEventHandler PropertyChanged;
            public void NotifyPropertyChanged( [CallerMemberName] string propertyName = null)
            {
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
            }
    
            private string _applicationStateText;
    
            public string ApplicationStateText
            {
                get { return _applicationStateText; }
                set
                {
                    if (_applicationStateText != value)
                    {
                        _applicationStateText = value;
                        NotifyPropertyChanged();
                    }
                }
            }
            public MainWindowViewModel()
            {
                ApplicationStateText = "Hello World!";
            }
        }
    }

and make sure, that our `Binding` of `TextBlock.Text` actually listens to a `PropertyChangedEvent`:

    <Window x:Class="Application.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
        xmlns:vm="clr-namespace:Application.ViewModels">
        <Window.DataContext>
           <vm:MainWindowViewModel/>
        </Window.DataContext>
        <Grid>
            <TextBlock Text={Binding Path=ApplicationStateText, UpdateSourceTrigger=PropertyChanged }" />
        </Grid>
    </Window>

## Binding to a Collection of Objects with INotifyPropertyChanged and INotifyCollectionChanged
Let's assume you have a `ListView` wich is supposed to display every `User` object listed under the `Users` Property of the `ViewModel` where Properties of the User object can get updated programatically.<br/>

    <ListView ItemsSource="{Binding Path=Users}" >
        <ListView.ItemTemplate>
            <DataTemplate DataType="{x:Type models:User}">
                <StackPanel Orientation="Horizontal">
                    <TextBlock Margin="5,3,15,3" 
                             Text="{Binding Id, Mode=OneWay}" />
                    <TextBox Width="200"
                             Text="{Binding Name, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged, Delay=450}"/>
                </StackPanel>
            </DataTemplate>
        </ListView.ItemTemplate>
    </ListView>

Despite for `INotifyPropertyChanged` beeing implemented correctly for the `User` object
<!-- language: lang-cs -->
    public class User : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        private int _id;
        private string _name;
    
        public int Id
        {
            get { return _id; }
            private set
            {
                if (_id == value) return;
                _id = value;
                NotifyPropertyChanged();
            }
        }
        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged();
            }
        }

        public User(int id, string name)
        {
            Id = id;
            Name = name;
        }
    
        private void NotifyPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }

and for your `ViewModel` object
<!-- language: lang-cs -->
    public sealed class MainWindowViewModel : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;
    
        private List<User> _users;
        public List<User> Users
        {
            get { return _users; }
            set
            {
                if (_users == value) return;
                _users = value;
                NotifyPropertyChanged();
            }
        }
        public MainWindowViewModel()
        {
            Users = new List<User> {new User(1, "John Doe"), new User(2, "Jane Doe"), new User(3, "Foo Bar")};
        }
    
        private void NotifyPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }

your UI wont update, if a change to a User is made programmatically.</p>
This is simply because you have only set INotifyPropertyChanged on the Instance of the List itself. Only if you completely re-instantiate the List if one property of an Element changes your UI will update.
<!-- language: lang-cs -->
    // DO NOT DO THIS
    User[] userCache = Users.ToArray();
    Users = new List<User>(userCache);
This however is very tiresome and unbelievably bad for performance.<br/>
If you have a List of 100'000 Elements showing both the ID and Name of the User, there will be 200'000 DataBindings in place wich each have to be re-created. This results in noticable Lag to the User whenever a change is made to anything.</p>
To partly solve this issue, you can use `System.ComponentModel.ObservableCollection<T>` instead of `List<T>`:
<!-- language: lang-cs -->
    private ObservableCollection<User> _users;
    public ObservableCollection<User> Users
    {
        get { return _users; }
        set
        {
            if (_users == value) return;
            _users = value;
            NotifyPropertyChanged();
        }
    }
The `ObservableCollection` provides us with the `CollectionChanged`Event and implements `INotifyPropertyChanged` itself. According to [MSDN][1] the Event will rise, "[..]when an item is *added*, *removed*, *changed*, *moved*, or the *entire list is refreshed*". <br/>You will however quickly come to realize that with .NET 4.5.2 and prior, the `ObservableCollection` will not raise a CollectionChanged Event if a Property of an Element in the Collection Changes as discussed [here][2]. </p>Following [this][3] solution we can simply implement our own `TrulyObservableCollection<T>` without the `INotifyPropertyChanged` constraint for `T` having everything we need and exposing wether `T` implements `INotifyPropertyChanged` or not:
<!-- language: lang-cs -->
    /*
     * Original Class by Simon @StackOverflow http://stackoverflow.com/a/5256827/3766034
     * Removal of the INPC-Constraint by Jirajha @StackOverflow 
     * according to to suggestion of nikeee @StackOverflow http://stackoverflow.com/a/10718451/3766034
     */
    public sealed class TrulyObservableCollection<T> : ObservableCollection<T>
    {
        private readonly bool _inpcHookup;
        public bool NotifyPropertyChangedHookup => _inpcHookup;

        public TrulyObservableCollection()
        {
            CollectionChanged += TrulyObservableCollectionChanged;
            _inpcHookup = typeof(INotifyPropertyChanged).GetTypeInfo().IsAssignableFrom(typeof(T));
        }
        public TrulyObservableCollection(IEnumerable<T> items) : this()
        {
            foreach (var item in items)
            {
                this.Add(item);
            }
        }

        private void TrulyObservableCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (NotifyPropertyChangedHookup && e.NewItems != null && e.NewItems.Count > 0)
            {
                foreach (INotifyPropertyChanged item in e.NewItems)
                {
                    item.PropertyChanged += ItemPropertyChanged;
                }
            }
            if (NotifyPropertyChangedHookup && e.OldItems != null && e.OldItems.Count > 0)
            {
                foreach (INotifyPropertyChanged item in e.OldItems)
                {
                    item.PropertyChanged -= ItemPropertyChanged;
                }
            }
        }
        private void ItemPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            var args = new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, sender, sender, IndexOf((T)sender));
            OnCollectionChanged(args);
        }
    }

and define our Property `Users` as `TrulyObservableCollection<User>` in our ViewModel
<!-- language: lang-cs -->
    private TrulyObservableCollection<string> _users;
    public TrulyObservableCollection<string> Users
    {
        get { return _users; }
        set
        {
            if (_users == value) return;
            _users = value;
            NotifyPropertyChanged();
        }
    }

Our UI will now get notified about once a INPC-Property of an element within the Collection changes without the need to re-create every single `Binding`.

  [1]: https://msdn.microsoft.com/de-de/library/ms668604%28v=vs.110%29.aspx#Anchor_5
  [2]: http://stackoverflow.com/questions/1427471/observablecollection-not-noticing-when-item-in-it-changes-even-with-inotifyprop
  [3]: http://stackoverflow.com/a/5256827/3766034

