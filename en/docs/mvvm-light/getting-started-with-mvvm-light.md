---
title: "Getting started with mvvm-light"
slug: "getting-started-with-mvvm-light"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## RelayCommand

The `RelayCommand` implements the `ICommand` interface and can therefore be used to bind to `Command`s in XAML (as the `Command` property of the `Button` element)

The constructor takes two arguments; the first is an `Action` which will be executed if `ICommand.Execute` is called (e.g. the user clicks on the button), the second one is a `Func<bool>` which determines if the action can be executed (defaults to true, called `canExecute` in the following paragraph). 

the basic structure is as follows:

    public ICommand MyCommand => new RelayCommand(
        () =>
        {
            //execute action
            Message = "clicked Button";
        },
        () =>
        {
            //return true if button should be enabled or not
            return true;
        }
    );

Some notable effects:
- If `canExecute` returns false, the `Button` will be disabled for the user
- Before the action is really executed, `canExecute` will be checked again 
- You can call `MyCommand.RaiseCanExecuteChanged();` to force reevaluation of the `canExecute` `Func` 


## RelayCommand<T>

The `RelayCommand<T>` is similar to the `RelayCommand`, but allows to directly pass an object to the command. It implements the `ICommand` interface and can therefore be used to bind to `Command`s in XAML (e.g. as the `Command` property of the `Button` element). You can then use the `CommandParameter` property to pass the object to the command.

XAML example:

    <Button Command="{Binding MyCommand}" CommandParameter="{Binding MyModel}" />

The constructor takes two arguments; the first is an Action<T> which will be executed if ICommand.Execute is called (e.g. the user clicks on the button), the second one is a Func<string,bool> which determines if the action can be executed (defaults to true, called canExecute in the following paragraph).
the basic structure is as follows:

    public RelayCommand<string> MyCommand => new RelayCommand<string>(
        obj =>
        {
            //execute action
            Message = obj;
        },
        obj =>
        {
            //return true if button should be enabled or not
            return obj != "allowed";
        }
    );

Some notable effects:
- If `canExecute` returns `false`, the `Button` will be disabled for the user
- Before the action is really executed, `canExecute` will be checked again
- You can call `MyCommand.RaiseCanExecuteChanged();` to force reevaluation of the `canExecute` `Func`

## ObservableObject
The `ObservableObject` class contains some helpful methods to help with the MVVM pattern.

The `RaisePropertyChanged` provides a compile safe method to raise property changed events.  
It can be called with

    RaisePropertyChanged(() => MyProperty);

The `Set` method can be used in the property setter to set the new value and raise the property changed event (only if change occurred). It returns `true` if change occurred and `false` otherwise.  
example usage:

    private string _myValue;
    public string MyValue    
    {
        get { return _myValue; }
        set { Set(ref _myValue, value); }
    }

## ViewModelBase
`ViewModelBase` extends `ObservableObject` and adds some methods useful for viewmodels.

The property `IsInDesignMode` or `IsInDesignModeStatic` allows to determine if the code is executed in the design mode (in Visual Studio Design View) or not. The two properties are exactly the same.



