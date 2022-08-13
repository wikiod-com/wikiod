---
title: "Annotation des données"
slug: "annotation-des-donnees"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Création d'un attribut de validation personnalisé



Des attributs de validation personnalisés peuvent être créés en dérivant de la classe de base `ValidationAttribute`, puis en remplaçant les méthodes `virtual` selon les besoins.

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

Cet attribut peut alors être utilisé comme ceci :

    public class Model
    {
        [NotABanana(ErrorMessage = "Bananas are not allowed.")]
        public string FavoriteFruit { get; set; }
    }

## Principes de base de l'annotation de données
Les annotations de données sont un moyen d'ajouter plus d'informations contextuelles aux classes ou aux membres d'une classe. Il existe trois grandes catégories d'annotations :
* Attributs de validation : ajouter des critères de validation aux données
* Afficher les attributs : spécifiez comment les données doivent être affichées pour l'utilisateur
* Attributs de modélisation : ajouter des informations sur l'utilisation et la relation avec d'autres classes

## Utilisation ##
Voici un exemple où deux `ValidationAttribute` et un `DisplayAttribute` sont utilisés :

    class Kid
    {
        [Range(0, 18)] // The age cannot be over 18 and cannot be negative
        public int Age { get; set; }
        [StringLength(MaximumLength = 50, MinimumLength = 3)] // The name cannot be under 3 chars or more than 50 chars
        public string Name { get; set; }
        [DataType(DataType.Date)] // The birthday will be displayed as a date only (without the time)
        public DateTime Birthday { get; set; }
    }

Les annotations de données sont principalement utilisées dans des frameworks tels que ASP.NET. Par exemple, dans `ASP.NET MVC`, lorsqu'un modèle est reçu par une méthode de contrôleur, `ModelState.IsValid()` peut être utilisé pour indiquer si le modèle reçu respecte tous ses `ValidationAttribute`. `DisplayAttribute` est également utilisé dans `ASP.NET MVC` pour déterminer comment afficher les valeurs sur une page Web.

## Exécuter manuellement les attributs de validation
La plupart du temps, les attributs de validation sont utilisés dans des frameworks (tels que ASP.NET). Ces frameworks prennent en charge l'exécution des attributs de validation. Mais que se passe-t-il si vous souhaitez exécuter les attributs de validation manuellement ? Utilisez simplement la classe `Validator` (aucune réflexion nécessaire).

## Contexte de validation ##
Toute validation a besoin d'un contexte pour donner des informations sur ce qui est validé. Cela peut inclure diverses informations telles que l'objet à valider, certaines propriétés, le nom à afficher dans le message d'erreur, etc.

    ValidationContext vc = new ValidationContext(objectToValidate); // The simplest form of validation context. It contains only a reference to the object being validated.

Une fois le contexte créé, il existe plusieurs façons de procéder à la validation.

## Valider un objet et toutes ses propriétés ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidateObject(objectToValidate, vc, results, true); // Validates the object and its properties using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Valider une propriété d'un objet ##
    ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
    bool isValid = Validator.TryValidatePropery(objectToValidate.PropertyToValidate, vc, results, true); // Validates the property using the previously created context.
    // The variable isValid will be true if everything is valid
    // The results variable contains the results of the validation

## Et plus ##
Pour en savoir plus sur la validation manuelle, consultez :
* [Documentation de la classe ValidationContext][1]
* [Documentation de classe de validateur][2]


[1] : https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationcontext(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validator(v=vs.110).aspx

## EditableAttribute (attribut de modélisation de données)
`EditableAttribute` définit si les utilisateurs doivent pouvoir modifier la valeur de la propriété de classe.

    public class Employee
    {
        [Editable(false)]
        public string FirstName { get; set; }
    }

**Exemple d'utilisation simple dans une application XAML**

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

<h>

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

<h>

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

<h>

[![modifiable][1]][1]


[1] : http://i.stack.imgur.com/ng8VJ.png

## Attributs de validation
<!-- language-all: lang-cs -->
Les attributs de validation sont utilisés pour appliquer diverses règles de validation de manière déclarative sur des classes ou des membres de classe. Tous les attributs de validation dérivent de la classe de base [ValidationAttribute][1].

-------------------------------------------------- ---------------------------------

Exemple : AttributRequis
--------------------------------
Lorsqu'il est validé via la méthode `ValidationAttribute.Validate`, cet attribut renverra une erreur si la propriété `Name` est nulle ou ne contient que des espaces.

    public class ContactModel
    {
        [Required(ErrorMessage = "Please provide a name.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ---------------------------------

Exemple : StringLengthAttribute
--------------------------------
Le `StringLengthAttribute` valide si une chaîne est inférieure à la longueur maximale d'une chaîne. Il peut éventuellement spécifier une longueur minimale. Les deux valeurs sont inclusives.

    public class ContactModel
    {
        [StringLength(20, MinimumLength = 5, ErrorMessage = "A name must be between five and twenty characters.")]
        public string Name { get; set; }
    }
-------------------------------------------------- ---------------------------------

Example: RangeAttribute
--------------------------------
Le `RangeAttribute` donne la valeur maximale et minimale pour un champ numérique.

    public class Model
    {
        [Range(0.01, 100.00,ErrorMessage = "Price must be between 0.01 and 100.00")]
        public decimal Price { get; set; }
    }
-------------------------------------------------- ---------------------------------

Exemple : CustomValidationAttribute
--------------------------------
La classe `CustomValidationAttribute` permet d'invoquer une méthode `statique` personnalisée pour la validation. La méthode personnalisée doit être `static ValidationResult [MethodName] (entrée d'objet)`.

    public class Model
    {
        [CustomValidation(typeof(MyCustomValidation), "IsNotAnApple")]
        public string FavoriteFruit { get; set; }
    }

Déclaration de méthode :

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

[1] : https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationattribute(v=vs.95).aspx

## DisplayNameAttribute (attribut d'affichage)
`DisplayName` définit le nom d'affichage d'une propriété, d'un événement ou d'une méthode void publique ayant zéro (0) argument.

    public class Employee
    {
        [DisplayName(@"Employee first name")]
        public string FirstName { get; set; }
    }

**Exemple d'utilisation simple dans une application XAML**

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

<h>

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

<h>

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

<h>

[![entrez la description de l'image ici][1]][1]


[1] : http://i.stack.imgur.com/XL60j.png

