---
title: "Anotación de datos"
slug: "anotacion-de-datos"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Crear un atributo de validación personalizado



Los atributos de validación personalizados se pueden crear derivando de la clase base `ValidationAttribute` y luego anulando los métodos `virtuales` según sea necesario.

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

Este atributo se puede utilizar así:

    public class Model
    {
        [NotABanana(ErrorMessage = "Bananas are not allowed.")]
        public string FavoriteFruit { get; set; }
    }

## Conceptos básicos de anotación de datos
Las anotaciones de datos son una forma de agregar más información contextual a las clases oa los miembros de una clase. Hay tres categorías principales de anotaciones:
* Atributos de validación: agregue criterios de validación a los datos
* Atributos de visualización: especifique cómo se deben mostrar los datos al usuario
* Atributos de modelado: agregue información sobre el uso y la relación con otras clases

## Uso ##
Aquí hay un ejemplo donde se usan dos `ValidationAttribute` y un `DisplayAttribute`:

    class Kid
    {
        [Range(0, 18)] // The age cannot be over 18 and cannot be negative
        public int Age { get; set; }
        [StringLength(MaximumLength = 50, MinimumLength = 3)] // The name cannot be under 3 chars or more than 50 chars
        public string Name { get; set; }
        [DataType(DataType.Date)] // The birthday will be displayed as a date only (without the time)
        public DateTime Birthday { get; set; }
    }

Las anotaciones de datos se utilizan principalmente en marcos como ASP.NET. Por ejemplo, en `ASP.NET MVC`, cuando un método de controlador recibe un modelo, se puede usar `ModelState.IsValid()` para saber si el modelo recibido respeta todos sus `ValidationAttribute`. `DisplayAttribute` también se usa en `ASP.NET MVC` para determinar cómo mostrar los valores en una página web.

## Ejecutar manualmente los atributos de validación
La mayoría de las veces, los atributos de validación se usan dentro de marcos (como ASP.NET). Esos marcos se encargan de ejecutar los atributos de validación. Pero, ¿qué sucede si desea ejecutar los atributos de validación manualmente? Simplemente use la clase `Validator` (no se necesita reflexión).

## Contexto de validación ##
Cualquier validación necesita un contexto para brindar información sobre lo que se está validando. Esto puede incluir información diversa, como el objeto a validar, algunas propiedades, el nombre para mostrar en el mensaje de error, etc.

    ValidationContext vc = new ValidationContext(objectToValidate); // The simplest form of validation context. It contains only a reference to the object being validated.

Una vez que se crea el contexto, existen múltiples formas de realizar la validación.

## Validar un objeto y todas sus propiedades ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidateObject(objectToValidate, vc, results, true); // Validates the object and its properties using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Validar una Propiedad de un Objeto ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidatePropery(objectToValidate.PropertyToValidate, vc, results, true); // Validates the property using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Y más ##
Para obtener más información sobre la validación manual, consulte:
* [Documentación de la clase ValidationContext][1]
* [Documentación de clase de validador][2]


[1]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationcontext(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validator(v=vs.110).aspx

## EditableAttribute (atributo de modelado de datos)
`EditableAttribute` establece si los usuarios deberían poder cambiar el valor de la propiedad de clase.

    public class Employee
    {
        [Editable(false)]
        public string FirstName { get; set; }
    }

**Ejemplo de uso simple en aplicación XAML**

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

<hr>

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

<hr>

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

<hr>

[![editable][1]][1]


[1]: http://i.stack.imgur.com/ng8VJ.png

## Atributos de validación
<!-- idioma-todo: lang-cs -->
Los atributos de validación se utilizan para hacer cumplir varias reglas de validación de manera declarativa en clases o miembros de clase. Todos los atributos de validación derivan de la clase base [ValidationAttribute][1].

-------------------------------------------------- ---------------------------------

Ejemplo: atributo requerido
--------------------------------
Cuando se valida mediante el método `ValidationAttribute.Validate`, este atributo devolverá un error si la propiedad `Name` es nula o solo contiene espacios en blanco.

    public class ContactModel
    {
        [Required(ErrorMessage = "Please provide a name.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ---------------------------------

Ejemplo: StringLengthAttribute
--------------------------------
El `StringLengthAttribute` valida si una cadena es menor que la longitud máxima de una cadena. Opcionalmente, puede especificar una longitud mínima. Ambos valores son inclusivos.

    public class ContactModel
    {
        [StringLength(20, MinimumLength = 5, ErrorMessage = "A name must be between five and twenty characters.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ---------------------------------

Ejemplo: atributo de rango
--------------------------------
El `RangeAttribute` da el valor máximo y mínimo para un campo numérico.

    public class Model
    {
        [Range(0.01, 100.00,ErrorMessage = "Price must be between 0.01 and 100.00")]
        public decimal Price { get; set; }
    }
-------------------------------------------------- ---------------------------------

Ejemplo: atributo de validación personalizado
--------------------------------
La clase `CustomValidationAttribute` permite invocar un método `estático` personalizado para la validación. El método personalizado debe ser `static ValidationResult [MethodName] (objeto de entrada)`.

    public class Model
    {
        [CustomValidation(typeof(MyCustomValidation), "IsNotAnApple")]
        public string FavoriteFruit { get; set; }
    }

Declaración de método:

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

## DisplayNameAttribute (atributo de visualización)
`DisplayName` establece el nombre para mostrar de una propiedad, evento o método de anulación pública que tiene cero (0) argumentos.

    public class Employee
    {
        [DisplayName(@"Employee first name")]
        public string FirstName { get; set; }
    }

**Ejemplo de uso simple en aplicación XAML**

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

<hr>

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

<hr>

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

<hr>

[![ingrese la descripción de la imagen aquí][1]][1]


[1]: http://i.stack.imgur.com/XL60j.png

