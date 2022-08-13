---
title: "Telerik WPF RadGridView"
slug: "telerik-wpf-radgridview"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## RowDoubleClick
It's a common request to be able to handle a double-click on a row in a RadGridView. The solution proposed by Telerik (http://demos.telerik.com/silverlight/#GridView/ClickEvents ) is based around code behind:

     this.grid.AddHandler(GridViewCellBase.CellDoubleClickEvent, new EventHandler<RadRoutedEventArgs>(OnCellDoubleClick), true);

if you're using MVVM, you would probably prefer to attach an ICommand, which will be passed the DataContext of the row that has been clicked on, as a parameter. Here's how to do that. XAML:

        <telerik:RadGridView
            ItemsSource="{Binding Rows}"
            IsReadOnly="True"
            ShowGroupPanel="False"
            ShowColumnFooters="True"
            local:RadGridViewAttachedProperties.RowDoubleClickCommand="{Binding DoubleClickRow}"/>

AttachedProperty:

    public static class RadGridViewAttachedProperties
    {
        public static readonly DependencyProperty RowDoubleClickCommandProperty =
            DependencyProperty.RegisterAttached("RowDoubleClickCommand", typeof(ICommand), typeof(RadGridViewAttachedProperties), new UIPropertyMetadata(null, OnRowDoubleClickCommandChanged));

        public static ICommand GetRowDoubleClickCommand(DependencyObject obj)
        {
            return (ICommand)obj.GetValue(RowDoubleClickCommandProperty);
        }

        public static void SetRowDoubleClickCommand(DependencyObject obj, ICommand value)
        {
            obj.SetValue(RowDoubleClickCommandProperty, value);
        }

        private static void OnMouseDoubleClick(object sender, RoutedEventArgs e)
        {
            var obj = sender as DependencyObject;
            if (obj != null)
            {
                var command = GetRowDoubleClickCommand(obj);
                if (command != null)
                {
                    var frameworkElement = e.OriginalSource as FrameworkElement;
                    var dataContext = frameworkElement?.DataContext;
                    if (command.CanExecute(dataContext))
                    {
                        command.Execute(dataContext);
                    }
                }
            }
        }

        private static void OnRowDoubleClickCommandChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var controlIsTheRightType = d as RadGridView;
            if (controlIsTheRightType == null)
            {
                return;
            }

            controlIsTheRightType.MouseDoubleClick += OnMouseDoubleClick;
        }
    }

Example ViewModel (uses a Telerik DelegateCommand as the command; but you can do what you like):

    public class MainWindowViewModel
    {
        public MainWindowViewModel()
        {
            Rows = new ObservableCollection<string>();
            Rows.Add("kljshndfoa");
            DoubleClickRow = new DelegateCommand(e => MessageBox.Show("Hello " + e));
        }

        public ICommand DoubleClickRow { get; }
        
        public ObservableCollection<string> Rows { get; }
    }

NOTE: you will know what the type of things in your RadGridView's ItemsSource are. In the Execute method of your ICommand, make sure you check what the type of the parameter to the command is, since clicking on the header or footer will also trigger this command; but in those scenarios, the parameter will not be one of your items.

