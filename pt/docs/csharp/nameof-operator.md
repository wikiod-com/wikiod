---
title: "nome do operador"
slug: "nome-do-operador"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

O operador `nameof` permite que você obtenha o nome de uma __variable__, __type__ ou __member__ em forma de string sem codificá-lo como um literal.

A operação é avaliada em tempo de compilação, o que significa que você pode renomear um identificador referenciado, usando o recurso de renomeação de um IDE, e a string de nome será atualizada com ele.

## Sintaxe
- nomede(expressão)

## Aumento do evento PropertyChanged
**Trecho**

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
      
**Saída do console**

> Endereço

## Uso básico: Imprimindo um nome de variável
O operador `nameof` permite que você obtenha o nome de uma variável, tipo ou membro em forma de string sem codificá-lo como um literal. A operação é avaliada em tempo de compilação, o que significa que você pode renomear, usando o recurso de renomeação de um IDE, um identificador referenciado e a string de nome será atualizada com ele.

    var myString = "String Contents";
    Console.WriteLine(nameof(myString));

Produziria

> minhaString

porque o nome da variável é "myString". Refatorar o nome da variável mudaria a string.

Se chamado em um tipo de referência, o operador `nameof` retorna o nome da referência atual, *não* o nome ou o nome do tipo do objeto subjacente. Por exemplo:

    string greeting = "Hello!";
    Object mailMessageBody = greeting;

    Console.WriteLine(nameof(greeting)); // Returns "greeting"
    Console.WriteLine(nameof(mailMessageBody)); // Returns "mailMessageBody", NOT "greeting"!

## Verificação de argumentos e cláusulas de proteção
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

Usar o recurso `nameof` facilita a refatoração dos parâmetros do método.

## Links de ação MVC fortemente tipados
Em vez do usual digitado livremente:

    @Html.ActionLink("Log in", "UserController", "LogIn")

Agora você pode fazer links de ação fortemente tipados:

    @Html.ActionLink("Log in", @typeof(UserController), @nameof(UserController.LogIn))

Agora, se você quiser refatorar seu código e renomear o método `UserController.LogIn` para `UserController.SignIn`, você não precisa se preocupar em procurar todas as ocorrências de strings. O compilador fará o trabalho.


## Lidando com eventos PropertyChanged
**Trecho**

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

**Saída do console**

> Título alterado para Tudo está pegando fogo e quebrado
>
> Status alterado para ShowStopper
    

## Aplicado a um parâmetro de tipo genérico
**Trecho**

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

**Saída do console**

> TItem
>
> Alguma Classe

## Imprimindo um nome de parâmetro
**Trecho**

    public void DoSomething(int paramValue)
    {
        Console.WriteLine(nameof(paramValue));
    }

    ...

    int myValue = 10;
    DoSomething(myValue);

**Saída do console**

> paramValue

## Aplicado a identificadores qualificados
**Trecho**

    Console.WriteLine(nameof(CompanyNamespace.MyNamespace));
    Console.WriteLine(nameof(MyClass));
    Console.WriteLine(nameof(MyClass.MyNestedClass));
    Console.WriteLine(nameof(MyNamespace.MyClass.MyNestedClass.MyStaticProperty));

**Saída do console**
> MyNamespace
>
> MinhaClasse
>
> MyNestedClass
>
> MyStaticProperty

