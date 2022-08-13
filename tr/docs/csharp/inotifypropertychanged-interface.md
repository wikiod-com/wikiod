---
title: "INotifyPropertyDeğiştirilmiş arayüz"
slug: "inotifypropertydegistirilmis-arayuz"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

Sınıfınızın özelliklerinde meydana gelen değişiklikleri raporlamasını sağlamanız gerektiğinde, `INotifyPropertyChanged` arayüzüne ihtiyaç duyulur. Arayüz, tek bir "PropertyChanged" olayını tanımlar.

XAML Binding ile "PropertyChanged" olayı otomatik olarak bağlanır, bu nedenle XAML Binding ile çalışmak için yalnızca görünüm modelinizde veya veri bağlamı sınıflarında INotifyPropertyChanged arabirimini uygulamanız gerekir.

## C# 6'da INotifyPropertyChanged'i Uygulamak
"INotifyPropertyChange" uygulaması, arabirim bir dize olarak özellik adının belirtilmesini gerektirdiğinden hataya açık olabilir. Uygulamayı daha sağlam hale getirmek için 'CallerMemberName' özniteliği kullanılabilir.

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

'INotifyPropertyChanged' uygulayan birkaç sınıfınız varsa, arabirim uygulamasını ve yardımcı yöntemi ortak temel sınıfa göre yeniden düzenlemeyi yararlı bulabilirsiniz:

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

## INotifyPropertyGenel Küme Yöntemiyle Değiştirildi
Aşağıdaki "NotifyPropertyChangedBase" sınıfı, türetilmiş herhangi bir türden çağrılabilen genel bir Set yöntemini tanımlar.

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

Bu genel Set yöntemini kullanmak için NotifyPropertyChangedBase'den türetilen bir sınıf oluşturmanız yeterlidir.

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


Yukarıda gösterildiği gibi, bir özelliğin ayarlayıcısında `Set(ref _fieldName, value);` öğesini çağırabilirsiniz ve gerekirse, otomatik olarak bir PropertyChanged olayı oluşturacaktır.

Daha sonra, özellik değişikliklerini işlemesi gereken başka bir sınıftan PropertyChanged olayına kaydolabilirsiniz.

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

