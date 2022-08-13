---
title: "Interface INotifyPropertyChangedINotifyPropertyChanged interface"
slug: "interface-inotifypropertychangedinotifypropertychanged-interface"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

L'interface `INotifyPropertyChanged` est nécessaire chaque fois que vous devez faire en sorte que votre classe signale les modifications apportées à ses propriétés. L'interface définit un seul événement `PropertyChanged`.

Avec XAML Binding, l'événement `PropertyChanged` est câblé automatiquement, vous n'avez donc qu'à implémenter l'interface INotifyPropertyChanged sur votre modèle de vue ou vos classes de contexte de données pour travailler avec XAML Binding.

## Implémentation de INotifyPropertyChanged en C# 6
L'implémentation de `INotifyPropertyChange` peut être sujette aux erreurs, car l'interface nécessite de spécifier le nom de la propriété sous forme de chaîne. Afin de rendre l'implémentation plus robuste, un attribut `CallerMemberName` peut être utilisé.

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

Si vous avez plusieurs classes implémentant `INotifyPropertyChanged`, vous trouverez peut-être utile de refactoriser l'implémentation de l'interface et la méthode d'assistance vers la classe de base commune :

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

## INotifyPropertyChanged avec la méthode de jeu générique
La classe `NotifyPropertyChangedBase` ci-dessous définit une méthode Set générique qui peut être appelée à partir de n'importe quel type dérivé.

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

Pour utiliser cette méthode Set générique, il vous suffit de créer une classe qui dérive de NotifyPropertyChangedBase.

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


Comme indiqué ci-dessus, vous pouvez appeler `Set(ref _fieldName, value);` dans le setter d'une propriété et cela déclenchera automatiquement un événement PropertyChanged si nécessaire.

Vous pouvez ensuite vous inscrire à l'événement PropertyChanged à partir d'une autre classe qui doit gérer les modifications de propriétés.

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

