---
title: "nombre del operador"
slug: "nombre-del-operador"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

El operador `nameof` le permite obtener el nombre de una __variable__, __tipo__ o __miembro__ en forma de cadena sin codificarlo como un literal.

La operación se evalúa en tiempo de compilación, lo que significa que puede cambiar el nombre de un identificador al que se hace referencia, utilizando la función de cambio de nombre de un IDE, y la cadena de nombre se actualizará con él.

## Sintaxis
- nombre de (expresión)

## Generar evento PropertyChanged
**Retazo**

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
      
**Salida de consola**

> Dirección

## Uso básico: Imprimir un nombre de variable
El operador `nameof` le permite obtener el nombre de una variable, tipo o miembro en forma de cadena sin codificarlo como un literal. La operación se evalúa en tiempo de compilación, lo que significa que puede cambiar el nombre, utilizando la función de cambio de nombre de un IDE, un identificador referenciado y la cadena de nombre se actualizará con él.

    var myString = "String Contents";
    Console.WriteLine(nameof(myString));

daría salida

> miCadena

porque el nombre de la variable es "myString". Refactorizar el nombre de la variable cambiaría la cadena.

Si se invoca en un tipo de referencia, el operador `nameof` devuelve el nombre de la referencia actual, *no* el nombre o nombre de tipo del objeto subyacente. Por ejemplo:

    string greeting = "Hello!";
    Object mailMessageBody = greeting;

    Console.WriteLine(nameof(greeting)); // Returns "greeting"
    Console.WriteLine(nameof(mailMessageBody)); // Returns "mailMessageBody", NOT "greeting"!

## Comprobación de argumentos y cláusulas de protección
Preferir

    public class Order
    {
        public OrderLine AddOrderLine(OrderLine orderLine)
        {
            if (orderLine == null) throw new ArgumentNullException(nameof(orderLine));
            ...
        }
    }

Sobre

    public class Order
    {
        public OrderLine AddOrderLine(OrderLine orderLine)
        {
            if (orderLine == null) throw new ArgumentNullException("orderLine");
            ...
        }
    }    

El uso de la función `nameof` facilita la refactorización de los parámetros del método.

## Enlaces de acción MVC fuertemente tipados
En lugar de lo habitual escrito libremente:

    @Html.ActionLink("Log in", "UserController", "LogIn")

Ahora puede hacer enlaces de acción fuertemente tipados:

    @Html.ActionLink("Log in", @typeof(UserController), @nameof(UserController.LogIn))

Ahora, si desea refactorizar su código y cambiar el nombre del método `UserController.LogIn` a `UserController.SignIn`, no necesita preocuparse por buscar todas las ocurrencias de cadenas. El compilador hará el trabajo.


## Manejo de eventos PropertyChanged
**Retazo**

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

**Salida de consola**

> Título cambiado a Todo está en llamas y roto
>
> Estado cambiado a ShowStopper
    

## Aplicado a un parámetro de tipo genérico
**Retazo**

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

**Salida de consola**

> TItem
>
> AlgunaClase

## Imprimir un nombre de parámetro
**Retazo**

    public void DoSomething(int paramValue)
    {
        Console.WriteLine(nameof(paramValue));
    }

    ...

    int myValue = 10;
    DoSomething(myValue);

**Salida de consola**

> paramValor

## Aplicado a identificadores calificados
**Retazo**

    Console.WriteLine(nameof(CompanyNamespace.MyNamespace));
    Console.WriteLine(nameof(MyClass));
    Console.WriteLine(nameof(MyClass.MyNestedClass));
    Console.WriteLine(nameof(MyNamespace.MyClass.MyNestedClass.MyStaticProperty));

**Salida de consola**
> Mi espacio de nombres
>
> Mi Clase
>
> MiClaseAnidada
>
> MiPropiedadEstático

