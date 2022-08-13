---
title: "Anotação de dados"
slug: "anotacao-de-dados"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Criando um atributo de validação personalizado



Atributos de validação personalizados podem ser criados derivando da classe base `ValidationAttribute` e, em seguida, substituindo os métodos `virtuais` conforme necessário.

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

Este atributo pode ser usado assim:

    public class Model
    {
        [NotABanana(ErrorMessage = "Bananas are not allowed.")]
        public string FavoriteFruit { get; set; }
    }

## Noções básicas de anotação de dados
As anotações de dados são uma maneira de adicionar mais informações contextuais a classes ou membros de uma classe. Existem três categorias principais de anotações:
* Atributos de validação: adicione critérios de validação aos dados
* Atributos de exibição: especifique como os dados devem ser exibidos para o usuário
* Atributos de modelagem: adiciona informações de uso e relacionamento com outras classes

## Uso ##
Aqui está um exemplo onde dois `ValidationAttribute` e um `DisplayAttribute` são usados:

    class Kid
    {
        [Range(0, 18)] // The age cannot be over 18 and cannot be negative
        public int Age { get; set; }
        [StringLength(MaximumLength = 50, MinimumLength = 3)] // The name cannot be under 3 chars or more than 50 chars
        public string Name { get; set; }
        [DataType(DataType.Date)] // The birthday will be displayed as a date only (without the time)
        public DateTime Birthday { get; set; }
    }

As anotações de dados são usadas principalmente em estruturas como ASP.NET. Por exemplo, em `ASP.NET MVC`, quando um modelo é recebido por um método controlador, `ModelState.IsValid()` pode ser usado para dizer se o modelo recebido respeita todos os seus `ValidationAttribute`. `DisplayAttribute` também é usado em `ASP.NET MVC` para determinar como exibir valores em uma página da web.

## Executar manualmente os atributos de validação
Na maioria das vezes, os atributos de validação são usados ​​dentro de estruturas (como ASP.NET). Esses frameworks cuidam da execução dos atributos de validação. Mas e se você quiser executar atributos de validação manualmente? Basta usar a classe `Validator` (sem necessidade de reflexão).

## Contexto de validação ##
Qualquer validação precisa de um contexto para fornecer algumas informações sobre o que está sendo validado. Isso pode incluir várias informações, como o objeto a ser validado, algumas propriedades, o nome a ser exibido na mensagem de erro, etc.

    ValidationContext vc = new ValidationContext(objectToValidate); // The simplest form of validation context. It contains only a reference to the object being validated.

Depois que o contexto é criado, existem várias maneiras de fazer a validação.

## Validar um objeto e todas as suas propriedades ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidateObject(objectToValidate, vc, results, true); // Validates the object and its properties using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Validar uma propriedade de um objeto ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidatePropery(objectToValidate.PropertyToValidate, vc, results, true); // Validates the property using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## E mais ##
Para saber mais sobre validação manual, consulte:
* [Documentação da classe ValidationContext][1]
* [Documentação da classe do validador][2]


[1]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationcontext(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validator(v=vs.110).aspx

## EditableAttribute (atributo de modelagem de dados)
`EditableAttribute` define se os usuários devem poder alterar o valor da propriedade de classe.

    public class Employee
    {
        [Editable(false)]
        public string FirstName { get; set; }
    }

**Exemplo de uso simples em aplicativo XAML**

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

[![editável][1]][1]


[1]: http://i.stack.imgur.com/ng8VJ.png

## Atributos de validação
<!-- language-all: lang-cs -->
Os atributos de validação são usados ​​para impor várias regras de validação de forma declarativa em classes ou membros de classe. Todos os atributos de validação derivam da classe base [ValidationAttribute][1].

-------------------------------------------------- ----------------------------------

Exemplo: RequiredAttribute
--------------------------------
Quando validado através do método `ValidationAttribute.Validate`, este atributo retornará um erro se a propriedade `Name` for nula ou contiver apenas espaços em branco.

    public class ContactModel
    {
        [Required(ErrorMessage = "Please provide a name.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ----------------------------------

Exemplo: StringLengthAttribute
--------------------------------
O `StringLengthAttribute` valida se uma string é menor que o comprimento máximo de uma string. Ele pode opcionalmente especificar um comprimento mínimo. Ambos os valores são inclusivos.

    public class ContactModel
    {
        [StringLength(20, MinimumLength = 5, ErrorMessage = "A name must be between five and twenty characters.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ----------------------------------

Exemplo: RangeAttribute
--------------------------------
O `RangeAttribute` fornece o valor máximo e mínimo para um campo numérico.

    public class Model
    {
        [Range(0.01, 100.00,ErrorMessage = "Price must be between 0.01 and 100.00")]
        public decimal Price { get; set; }
    }
-------------------------------------------------- ----------------------------------

Exemplo: CustomValidationAttribute
--------------------------------
A classe `CustomValidationAttribute` permite que um método `static` personalizado seja invocado para validação. O método personalizado deve ser `static ValidationResult [MethodName] (entrada do objeto)`.

    public class Model
    {
        [CustomValidation(typeof(MyCustomValidation), "IsNotAnApple")]
        public string FavoriteFruit { get; set; }
    }

Declaração do método:

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

## DisplayNameAttribute (atributo de exibição)
`DisplayName` define o nome de exibição para uma propriedade, evento ou método void público com zero (0) argumentos.

    public class Employee
    {
        [DisplayName(@"Employee first name")]
        public string FirstName { get; set; }
    }

**Exemplo de uso simples em aplicativo XAML**

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

[![digite a descrição da imagem aqui][1]][1]


[1]: http://i.stack.imgur.com/XL60j.png

