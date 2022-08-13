---
title: "Interface INotifyPropertyChanged"
slug: "interface-inotifypropertychanged"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

A interface `INotifyPropertyChanged` é necessária sempre que você precisar fazer com que sua classe relate as mudanças que estão acontecendo em suas propriedades. A interface define um único evento `PropertyChanged`.

Com XAML Binding, o evento `PropertyChanged` é conectado automaticamente, então você só precisa implementar a interface INotifyPropertyChanged em seu modelo de exibição ou classes de contexto de dados para trabalhar com XAML Binding.

## Implementando INotifyPropertyChanged em C# 6
A implementação de `INotifyPropertyChange` pode ser propensa a erros, pois a interface requer a especificação do nome da propriedade como uma string. Para tornar a implementação mais robusta, um atributo `CallerMemberName` pode ser usado.

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

Se você tiver várias classes implementando `INotifyPropertyChanged`, pode ser útil refatorar a implementação da interface e o método auxiliar para a classe base comum:

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

## INotifyPropertyChanged With Generic Set Method
A classe `NotifyPropertyChangedBase` abaixo define um método Set genérico que pode ser chamado de qualquer tipo derivado.

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

Para usar esse método Set genérico, basta criar uma classe que deriva de NotifyPropertyChangedBase.

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


Como mostrado acima, você pode chamar `Set(ref _fieldName, value);` no setter de uma propriedade e ele irá gerar automaticamente um evento PropertyChanged se for necessário.

Em seguida, você pode se registrar no evento PropertyChanged de outra classe que precisa manipular as alterações de propriedade.

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

