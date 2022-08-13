---
title: "nom de l'opérateur"
slug: "nom-de-loperateur"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

L'opérateur `nameof` vous permet d'obtenir le nom d'une __variable__, __type__ ou __member__ sous forme de chaîne sans le coder en dur comme un littéral.

L'opération est évaluée au moment de la compilation, ce qui signifie que vous pouvez renommer un identifiant référencé, en utilisant la fonctionnalité de renommage d'un IDE, et la chaîne de nom sera mise à jour avec.

## Syntaxe
- nomde(expression)

## Lever l'événement PropertyChanged
**Fragment**

    public class Person : INotifyPropertyChanged
    {
        private string _address;

        public event PropertyChangedEventHandler PropertyChanged;

        private void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        public string Address
        {
            get { return _address; }
            set
            {
                if (_address == value)
                {
                    return;
                }

                _address = value;
                OnPropertyChanged(nameof(Address));
            }
        }
    }

    ...

    var person = new Person();
    person.PropertyChanged += (s,e) => Console.WriteLine(e.PropertyName);

    person.Address = "123 Fake Street";
      
**Sortie console**

> Adresse

## Utilisation de base : impression d'un nom de variable
L'opérateur `nameof` vous permet d'obtenir le nom d'une variable, d'un type ou d'un membre sous forme de chaîne sans le coder en dur en tant que littéral. L'opération est évaluée au moment de la compilation, ce qui signifie que vous pouvez renommer, en utilisant la fonctionnalité de renommage d'un IDE, un identifiant référencé et la chaîne de nom sera mise à jour avec.

    var myString = "String Contents";
    Console.WriteLine(nameof(myString));

Sortirait

> machaîne

car le nom de la variable est "myString". Refactoriser le nom de la variable changerait la chaîne.

S'il est appelé sur un type de référence, l'opérateur `nameof` renvoie le nom de la référence actuelle, *pas* le nom ou le nom du type de l'objet sous-jacent. Par exemple:

    string greeting = "Hello!";
    Object mailMessageBody = greeting;

    Console.WriteLine(nameof(greeting)); // Returns "greeting"
    Console.WriteLine(nameof(mailMessageBody)); // Returns "mailMessageBody", NOT "greeting"!

## Vérification des arguments et clauses de garde
Préfère

    public class Order
    {
        public OrderLine AddOrderLine(OrderLine orderLine)
        {
            if (orderLine == null) throw new ArgumentNullException(nameof(orderLine));
            ...
        }
    }

Plus de

    public class Order
    {
        public OrderLine AddOrderLine(OrderLine orderLine)
        {
            if (orderLine == null) throw new ArgumentNullException("orderLine");
            ...
        }
    }    

L'utilisation de la fonctionnalité `nameof` facilite la refactorisation des paramètres de méthode.

## Liens d'action MVC fortement typés
Au lieu de l'habituel vaguement tapé :

    @Html.ActionLink("Log in", "UserController", "LogIn")

Vous pouvez maintenant faire des liens d'action fortement typés :

    @Html.ActionLink("Log in", @typeof(UserController), @nameof(UserController.LogIn))

Maintenant, si vous souhaitez refactoriser votre code et renommer la méthode `UserController.LogIn` en `UserController.SignIn`, vous n'avez pas à vous soucier de rechercher toutes les occurrences de chaîne. Le compilateur fera le travail.


## Gestion des événements PropertyChanged
**Fragment**

    public class BugReport : INotifyPropertyChanged
    {
        public string Title { ... }
        public BugStatus Status { ... }
    }

    ...

    private void BugReport_PropertyChanged(object sender, PropertyChangedEventArgs e)
    {
        var bugReport = (BugReport)sender;

        switch (e.PropertyName)
        {
            case nameof(bugReport.Title):
                Console.WriteLine("{0} changed to {1}", e.PropertyName, bugReport.Title);
                break;

            case nameof(bugReport.Status):
                Console.WriteLine("{0} changed to {1}", e.PropertyName, bugReport.Status);
                break;
        }
    }

    ...

    var report = new BugReport();
    report.PropertyChanged += BugReport_PropertyChanged;

    report.Title = "Everything is on fire and broken";
    report.Status = BugStatus.ShowStopper;

**Sortie console**

> Titre changé en Tout est en feu et brisé
>
> Statut changé en ShowStopper
    

## Appliqué à un paramètre de type générique
**Fragment**

    public class SomeClass<TItem>
    {
        public void PrintTypeName()
        {
            Console.WriteLine(nameof(TItem));
        }
    }

    ...

    var myClass = new SomeClass<int>();
    myClass.PrintTypeName();

    Console.WriteLine(nameof(SomeClass<int>));

**Sortie console**

> TItem
>
> UneClasse

## Impression d'un nom de paramètre
**Fragment**

    public void DoSomething(int paramValue)
    {
        Console.WriteLine(nameof(paramValue));
    }

    ...

    int myValue = 10;
    DoSomething(myValue);

**Sortie console**

> valeurParam

## Appliqué aux identifiants qualifiés
**Fragment**

    Console.WriteLine(nameof(CompanyNamespace.MyNamespace));
    Console.WriteLine(nameof(MyClass));
    Console.WriteLine(nameof(MyClass.MyNestedClass));
    Console.WriteLine(nameof(MyNamespace.MyClass.MyNestedClass.MyStaticProperty));

**Sortie console**
> Mon espace de noms
>
> MaClasse
>
> MaClasse Imbriquée
>
> MaPropriétéStatique

