---
title: "Veri Açıklaması"
slug: "veri-acklamas"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Özel bir doğrulama özelliği oluşturma



Özel doğrulama nitelikleri, "ValidationAttribute" temel sınıfından türetilerek ve ardından gerektiğinde "sanal" yöntemleri geçersiz kılarak oluşturulabilir.

    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = false)]
    public class NotABananaAttribute : ValidationAttribute
    {
        public override bool IsValid(object value)
        {
            var inputValue = value as string;
            var isValid = true;

            if (!string.IsNullOrEmpty(inputValue))
            {
                isValid = inputValue.ToUpperInvariant() != "BANANA";
            }

            return isValid;
        }
    }

Bu öznitelik daha sonra şu şekilde kullanılabilir:

    public class Model
    {
        [NotABanana(ErrorMessage = "Bananas are not allowed.")]
        public string FavoriteFruit { get; set; }
    }

## Veri Açıklaması Temelleri
Veri açıklamaları, sınıflara veya bir sınıfın üyelerine daha fazla bağlamsal bilgi eklemenin bir yoludur. Ek açıklamaların üç ana kategorisi vardır:
* Doğrulama Nitelikleri: verilere doğrulama kriterleri ekleyin
* Görüntü Nitelikleri: verilerin kullanıcıya nasıl görüntüleneceğini belirtin
* Modelleme Nitelikleri: kullanım ve diğer sınıflarla ilişki hakkında bilgi ekleyin

## Kullanım ##
Burada iki "ValidationAttribute" ve bir "DisplayAttribute"un kullanıldığı bir örnek verilmiştir:

    class Kid
    {
        [Range(0, 18)] // The age cannot be over 18 and cannot be negative
        public int Age { get; set; }
        [StringLength(MaximumLength = 50, MinimumLength = 3)] // The name cannot be under 3 chars or more than 50 chars
        public string Name { get; set; }
        [DataType(DataType.Date)] // The birthday will be displayed as a date only (without the time)
        public DateTime Birthday { get; set; }
    }

Veri açıklamaları çoğunlukla ASP.NET gibi çerçevelerde kullanılır. Örneğin, "ASP.NET MVC"de, bir denetleyici yöntemi tarafından bir model alındığında, alınan modelin tüm "ValidationAttribute" değerlerine uyup uymadığını söylemek için "ModelState.IsValid()" kullanılabilir. 'DisplayAttribute', bir web sayfasında değerlerin nasıl görüntüleneceğini belirlemek için 'ASP.NET MVC'de de kullanılır.

## Doğrulama Niteliklerini Manuel Olarak Yürütün
Çoğu zaman, doğrulama öznitelikleri çerçeveler içinde kullanılır (ASP.NET gibi). Bu çerçeveler, doğrulama niteliklerini yürütmeye özen gösterir. Ancak doğrulama niteliklerini manuel olarak yürütmek isterseniz ne olur? Sadece 'Validator' sınıfını kullanın (yansıtma gerekmez).

## Doğrulama Bağlamı ##
Herhangi bir doğrulama, neyin doğrulandığı hakkında bazı bilgiler vermek için bir bağlama ihtiyaç duyar. Bu, doğrulanacak nesne, bazı özellikler, hata mesajında ​​görüntülenecek ad vb. gibi çeşitli bilgileri içerebilir.

    ValidationContext vc = new ValidationContext(objectToValidate); // The simplest form of validation context. It contains only a reference to the object being validated.

Bağlam oluşturulduktan sonra doğrulama yapmanın birden çok yolu vardır.

## Bir Nesneyi ve Tüm Özelliklerini Doğrulayın ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidateObject(objectToValidate, vc, results, true); // Validates the object and its properties using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Bir Nesnenin Özelliğini Doğrulayın ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidatePropery(objectToValidate.PropertyToValidate, vc, results, true); // Validates the property using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Ve dahası ##
Manuel doğrulama hakkında daha fazla bilgi için bkz.
* [ValidationContext Sınıf Belgeleri][1]
* [Doğrulayıcı Sınıfı Belgeleri][2]


[1]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationcontext(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validator(v=vs.110).aspx

## EditableAttribute (veri modelleme özelliği)
"EditableAttribute", kullanıcıların class özelliğinin değerini değiştirip değiştiremeyeceğini ayarlar.

    public class Employee
    {
        [Editable(false)]
        public string FirstName { get; set; }
    }

**XAML uygulamasında basit kullanım örneği**

    <Window x:Class="WpfApplication.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:wpfApplication="clr-namespace:WpfApplication"
            Height="70" Width="360" Title="Display name example">
    
        <Window.Resources>
            <wpfApplication:EditableConverter x:Key="EditableConverter"/>
        </Window.Resources>
    
        <StackPanel Margin="5">
            <!-- TextBox Text (FirstName property value) -->
            <!-- TextBox IsEnabled (Editable attribute) -->
            <TextBox Text="{Binding Employee.FirstName, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}" 
                     IsEnabled="{Binding Employee, Converter={StaticResource EditableConverter}, ConverterParameter=FirstName}"/>
        </StackPanel>
        
    </Window>

<saat>

    namespace WpfApplication
    {
        /// <summary>
        /// Interaction logic for MainWindow.xaml
        /// </summary>
        public partial class MainWindow : Window
        {
            private Employee _employee = new Employee() { FirstName = "This is not editable"};
    
            public MainWindow()
            {
                InitializeComponent();
                DataContext = this;
            }
    
            public Employee Employee
            {
                get { return _employee; }
                set { _employee = value; }
            }
        }
    }

<saat>

    namespace WpfApplication
    {
        public class EditableConverter : IValueConverter
        {
            public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
            {
                // return editable attribute's value for given instance property,
                // defaults to true if not found
                var attribute = value.GetType()
                    .GetProperty(parameter.ToString())
                    .GetCustomAttributes(false)
                    .OfType<EditableAttribute>()
                    .FirstOrDefault();
    
                return attribute != null ? attribute.AllowEdit : true;
            }
    
            public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            {
                throw new NotImplementedException();
            }
        }
    }

<saat>

[![düzenlenebilir][1]][1]


[1]: http://i.stack.imgur.com/ng8VJ.png

## Doğrulama Nitelikleri
<!-- tüm diller: lang-cs -->
Doğrulama öznitelikleri, çeşitli doğrulama kurallarını sınıflar veya sınıf üyeleri üzerinde bildirimsel bir biçimde uygulamak için kullanılır. Tüm doğrulama öznitelikleri [ValidationAttribute][1] temel sınıftan türetilir.

-------------------------------------------------- ---------------------------------

Örnek: RequiredAttribute
--------------------------------
'ValidationAttribute.Validate' yöntemiyle doğrulandığında, 'Ad' özelliği boşsa veya yalnızca boşluk içeriyorsa bu öznitelik bir hata döndürür.

    public class ContactModel
    {
        [Required(ErrorMessage = "Please provide a name.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ---------------------------------

Örnek: StringLengthAttribute
--------------------------------
"StringLengthAttribute", bir dizenin bir dizenin maksimum uzunluğundan küçük olup olmadığını doğrular. İsteğe bağlı olarak minimum bir uzunluk belirtebilir. Her iki değer de dahildir.

    public class ContactModel
    {
        [StringLength(20, MinimumLength = 5, ErrorMessage = "A name must be between five and twenty characters.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ---------------------------------

Örnek: RangeAttribute
--------------------------------
"RangeAttribute", sayısal bir alan için maksimum ve minimum değeri verir.

    public class Model
    {
        [Range(0.01, 100.00,ErrorMessage = "Price must be between 0.01 and 100.00")]
        public decimal Price { get; set; }
    }
-------------------------------------------------- ---------------------------------

Örnek: CustomValidationAttribute
--------------------------------
"CustomValidationAttribute" sınıfı, doğrulama için özel bir "statik" yöntemin çağrılmasına izin verir. Özel yöntem "statik ValidationResult [MethodName] (nesne girişi)" olmalıdır.

    public class Model
    {
        [CustomValidation(typeof(MyCustomValidation), "IsNotAnApple")]
        public string FavoriteFruit { get; set; }
    }

Yöntem bildirimi:

    public static class MyCustomValidation
    {
        public static ValidationResult IsNotAnApple(object input)
        {
            var result = ValidationResult.Success;

            if (input?.ToString()?.ToUpperInvariant() == "APPLE")
            {
                result = new ValidationResult("Apples are not allowed.");
            }

            return result;
        }
    }

[1]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationattribute(v=vs.95).aspx

## DisplayNameAttribute (görüntüleme özelliği)
"DisplayName", sıfır (0) bağımsız değişkeni olan bir özellik, olay veya genel geçersiz yöntemi için görünen adı ayarlar.

    public class Employee
    {
        [DisplayName(@"Employee first name")]
        public string FirstName { get; set; }
    }

**XAML uygulamasında basit kullanım örneği**

    <Window x:Class="WpfApplication.MainWindow"
            xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
            xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
            xmlns:wpfApplication="clr-namespace:WpfApplication"
            Height="100" Width="360" Title="Display name example">
    
        <Window.Resources>
            <wpfApplication:DisplayNameConverter x:Key="DisplayNameConverter"/>
        </Window.Resources>
    
        <StackPanel Margin="5">
            <!-- Label (DisplayName attribute) -->
            <Label Content="{Binding Employee, Converter={StaticResource DisplayNameConverter}, ConverterParameter=FirstName}" />
            <!-- TextBox (FirstName property value) -->
            <TextBox Text="{Binding Employee.FirstName, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}" />
        </StackPanel>
        
    </Window>

<saat>

    namespace WpfApplication
    {
        /// <summary>
        /// Interaction logic for MainWindow.xaml
        /// </summary>
        public partial class MainWindow : Window
        {
            private Employee _employee = new Employee();
    
            public MainWindow()
            {
                InitializeComponent();
                DataContext = this;
            }
    
            public Employee Employee
            {
                get { return _employee; }
                set { _employee = value; }
            }
        }
    }

<saat>

    namespace WpfApplication
    {
        public class DisplayNameConverter : IValueConverter
        {
            public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
            {
                // Get display name for given instance type and property name
                var attribute = value.GetType()
                    .GetProperty(parameter.ToString())
                    .GetCustomAttributes(false)
                    .OfType<DisplayNameAttribute>()
                    .FirstOrDefault();
    
                return attribute != null ? attribute.DisplayName : string.Empty;
            }
    
            public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            {
                throw new NotImplementedException();
            }
        }
    }

<saat>

[![buraya resim açıklamasını girin][1]][1]


[1]: http://i.stack.imgur.com/XL60j.png

