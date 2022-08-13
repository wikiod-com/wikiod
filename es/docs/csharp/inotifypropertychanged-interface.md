---
title: "Interfaz INotifyPropertyChanged"
slug: "interfaz-inotifypropertychanged"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

La interfaz `INotifyPropertyChanged` es necesaria siempre que necesite hacer que su clase informe los cambios que ocurren en sus propiedades. La interfaz define un solo evento `PropertyChanged`.

Con XAML Binding, el evento `PropertyChanged` se conecta automáticamente, por lo que solo necesita implementar la interfaz INotifyPropertyChanged en su modelo de vista o clases de contexto de datos para trabajar con XAML Binding.

## Implementando INotifyPropertyChanged en C# 6
La implementación de `INotifyPropertyChange` puede ser propensa a errores, ya que la interfaz requiere especificar el nombre de la propiedad como una cadena. Para hacer la implementación más robusta, se puede usar un atributo `CallerMemberName`.

    class C : INotifyPropertyChanged
    {
        // backing field
        int offset;
        // property
        public int Offset
        {
            get
            {
                return offset;
            }
            set
            {
                if (offset == value)
                    return;
                offset = value;
                RaisePropertyChanged();
            }
        }

        // helper method for raising PropertyChanged event
        void RaisePropertyChanged([CallerMemberName] string propertyName = null) =>
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));

        // interface implemetation
        public event PropertyChangedEventHandler PropertyChanged;
    }

Si tiene varias clases que implementan `INotifyPropertyChanged`, puede que le resulte útil refactorizar la implementación de la interfaz y el método auxiliar a la clase base común:

    class NotifyPropertyChangedImpl : INotifyPropertyChanged
    {
        protected void RaisePropertyChanged([CallerMemberName] string propertyName = null) =>
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));

        // interface implemetation
        public event PropertyChangedEventHandler PropertyChanged;
    }

    class C : NotifyPropertyChangedImpl
    {
        int offset;
        public int Offset
        {
            get { return offset; }
            set { if (offset != value) { offset = value; RaisePropertyChanged(); } }
        }
    }

## INotifyPropertyChanged con método de conjunto genérico
La siguiente clase `NotifyPropertyChangedBase` define un método Set genérico al que se puede llamar desde cualquier tipo derivado.

    public class NotifyPropertyChangedBase : INotifyPropertyChanged
    {
        protected void RaisePropertyChanged([CallerMemberName] string propertyName = null) =>
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        
        public event PropertyChangedEventHandler PropertyChanged;

        public virtual bool Set<T>(ref T field, T value, [CallerMemberName] string propertyName = null)
        {
            if (Equals(field, value))
                return false;
            storage = value;
            RaisePropertyChanged(propertyName);
            return true;
        }
    }

Para usar este método Set genérico, simplemente necesita crear una clase que derive de NotifyPropertyChangedBase.

    public class SomeViewModel : NotifyPropertyChangedBase
    {
        private string _foo;
        private int _bar;

        public string Foo
        {
            get { return _foo; }
            set { Set(ref _foo, value); }
        }

        public int Bar
        {
            get { return _bar; }
            set { Set(ref _bar, value); }
        }
    }


Como se muestra arriba, puede llamar a `Set(ref _fieldName, value);` en el setter de una propiedad y generará automáticamente un evento PropertyChanged si es necesario.

A continuación, puede registrarse en el evento PropertyChanged desde otra clase que necesite controlar los cambios de propiedad.

    public class SomeListener
    {
        public SomeListener()
        {
            _vm = new SomeViewModel();
            _vm.PropertyChanged += OnViewModelPropertyChanged;
        }

        private void OnViewModelPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            Console.WriteLine($"Property {e.PropertyName} was changed.");
        }

        private readonly SomeViewModel _vm;

    }

