---
title: "MVC, MVVM, MVP"
slug: "mvc-mvvm-mvp"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

It can be argued that MVC and related patterns are actually software architecture patterns rather than software design patterns.

## Model View Controller (MVC)

**1. What is MVC?** 
   
The Model View Controller (MVC) Pattern is a design pattern most commonly used for  creating user interfaces. The major advantage of MVC is that it separates:
  - the internal representation of the application state (the Model), 
  - how the information is presented to the user (the View), and 
  - the logic which controls how the user interacts with the application state (the Controller).

**2. Use cases of MVC**

The primary use case for MVC is in Graphical User Interface (GUI) programming. The View component listens to the Model component for changes. The Model acts as a broadcaster; when there is a change mode to the Model, it broadcasts its changes to the View and the Controller. The Controller is used by the View to modify the Model Component. 

**3. Implementation**

Consider the following implementation of MVC, where we have a Model class called `Animals`, a View class called
`DisplayAnimals`, and a controller class called `AnimalController`. The example below is a modified version of the tutorial 
on MVC from [Design Patterns - MVC Pattern][1].

    /* Model class */
    public class Animals {
        private String name;
        private String gender;

        public String getName() {
            return name;
        }

        public String getGender() {
            return gender;
        }

        public void setName(String name) {
             this.name = name;
        }

        public void setGender(String gender) {
            this.gender = gender;
        }
    }

    /* View class */
    public class DisplayAnimals {
        public void printAnimals(String tag, String gender) {
            System.out.println("My Tag name for Animal:" + tag);
            System.out.println("My gender: " + gender);
        }
    }

    /* Controller class */
    public class AnimalController {
         private Animal model;
         private DisplayAnimals view;

       public AnimalController(Animal model, DisplayAnimals view) {
          this.model = model;
          this.view = view;
       }
    
       public void setAnimalName(String name) {
          model.setName(name);        
       }
    
       public String getAnimalName() {
          return model.getName();        
       }
    
       public void setAnimalGender(String animalGender) {
          model.setGender(animalGender);        
       }
    
       public String getGender() {
          return model.getGender();        
       }
    
       public void updateView() {                
          view.printAnimals(model.getName(), model.getGender());
       }    
    }

**4. Sources used:**

[Design Patterns - MVC Pattern][1]

[Java SE Application Design With MVC][2]

[Model–view–controller][3]



 


  [1]: http://www.tutorialspoint.com/design_pattern/mvc_pattern.htm
  [2]: http://www.oracle.com/technetwork/articles/javase/index-142890.html
  [3]: https://en.wikipedia.org/wiki/Model-view-controller

## Model View ViewModel (MVVM)
**1. What is MVVM?**

The Model View ViewModel (MVVM) pattern is a design pattern most commonly used for creating user interfaces. It is derived from the the popular "Model View Controller" (MVC) pattern. The major advantage of MVVM is that it separates:

 - The internal representation of the application state (the Model).
 - How the information is presented to the user (the View).
 - The "value converter logic" responsible for exposing and converting the data from the model so that the data can be easily managed and presented in the view (the ViewModel).
 

**2. Use cases of MVVM**

The primary use case of MVVM is Graphical User Interface (GUI) programming. It is used to simply event-driven programming of user interfaces  by separating the view layer from the backend logic managing the data. 

In Windows Presentation Foundation (WPF), for example,  the view is designed using the framework markup language XAML. The XAML files are bound to ViewModels using data binding.  This way the view is only responsible for presentation and the viewmodel is only responsible for managing application state by working on the data in the model.
  
It is also used in the JavaScript library KnockoutJS.


**3. Implementation**

Consider the following implementation of MVVM using C# .Net and WPF. We have a Model class called Animals, a View class implemented in Xaml and a ViewModel called AnimalViewModel. The example below is a modified version of the tutorial on MVC from [Design Patterns - MVC Pattern][1].

Look how the Model does not know about anything, the ViewModel only knows about the Model and the View only knows about the ViewModel. 

The OnNotifyPropertyChanged-event enables updating both the model and the view so that when you enter something in the textbox in the view the model is updated. And if something updates the model, the view is updated.

    /*Model class*/
    public class Animal 
    {
        public string Name { get; set; }

        public string Gender { get; set; }
    }

    /*ViewModel class*/
    public class AnimalViewModel : INotifyPropertyChanged
    {
        private Animal _model;

        public AnimalViewModel()
        {
            _model = new Animal {Name = "Cat", Gender = "Male"};
        }

        public string AnimalName
        {
            get { return _model.Name; }
            set
            {
                _model.Name = value;
                OnPropertyChanged("AnimalName");
            }
        }

        public string AnimalGender
        {
            get { return _model.Gender; }
            set
            {
                _model.Gender = value;
                OnPropertyChanged("AnimalGender");
            }
        }

        //Event binds view to ViewModel.
        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged(string propertyName)
        {
            if (this.PropertyChanged != null)
            {
                var e = new PropertyChangedEventArgs(propertyName);
                this.PropertyChanged(this, e);
            }
        }
    }


    <!-- Xaml View -->
    <Window x:Class="WpfApplication6.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            Title="MainWindow" Height="350" Width="525"
            xmlns:viewModel="clr-namespace:WpfApplication6">
        
        <Window.DataContext>
            <viewModel:AnimalViewModel/>
        </Window.DataContext>
        
        <StackPanel>
            <TextBox Text="{Binding AnimalName}" Width="120" />
            <TextBox Text="{Binding AnimalGender}" Width="120" />
        </StackPanel>
    </Window>

**4. Sources used:**


[Model–view–viewmodel][2]

[A Simple MVVM Example][3]

[The World's Simplest C# WPF MVVM Example][4]

[The MVVM Pattern][5]


  [1]: http://www.tutorialspoint.com/design_pattern/mvc_pattern.htm
  [2]: https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93viewmodel
  [3]: https://rachel53461.wordpress.com/2011/05/08/simplemvvmexample/
  [4]: http://www.markwithall.com/programming/2013/03/01/worlds-simplest-csharp-wpf-mvvm-example.html
  [5]: https://msdn.microsoft.com/en-us/library/hh848246.aspx

