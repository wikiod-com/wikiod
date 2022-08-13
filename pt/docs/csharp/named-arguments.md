---
title: "Argumentos nomeados"
slug: "argumentos-nomeados"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## A ordem dos argumentos não é necessária
Você pode colocar argumentos nomeados em qualquer ordem que desejar.

Método de Amostra:

    public static string Sample(string left, string right)
    {
         return string.Join("-",left,right);
    }

Exemplo de chamada:

    Console.WriteLine (Sample(left:"A",right:"B"));
    Console.WriteLine (Sample(right:"A",left:"B"));

Resultados:

    A-B
    B-A
    


## Argumentos nomeados podem tornar seu código mais claro
Considere esta classe simples:

    class SmsUtil
    {
        public bool SendMessage(string from, string to, string message, int retryCount, object attachment)
        {
             // Some code
        }
    }

Antes do C# 3.0 era:

    var result = SmsUtil.SendMessage("Mehran", "Maryam", "Hello there!", 12, null);

você pode tornar essa chamada de método ainda mais clara com **argumentos nomeados**:

    var result = SmsUtil.SendMessage(
        from: "Mehran",
        to:  "Maryam",
        message "Hello there!",
        retryCount: 12,
        attachment: null);


## Argumentos nomeados e parâmetros opcionais
Você pode combinar argumentos nomeados com parâmetros opcionais.

Vamos ver este método:

    
    public sealed class SmsUtil
    {
        public static bool SendMessage(string from, string to, string message, int retryCount = 5, object attachment = null)
        {
             // Some code
        }
    }

Quando você quiser chamar este método *sem*, defina o argumento `retryCount`:


    var result = SmsUtil.SendMessage(
                            from       : "Cihan",
                            to         : "Yakar",
                            message    : "Hello there!",
                            attachment : new object());

## Argumentos nomeados evita bugs em parâmetros opcionais
Sempre use argumentos nomeados para parâmetros opcionais, para evitar possíveis bugs quando o método for modificado.

    class Employee
    {
        public string Name { get; private set; }

        public string Title { get; set; }

        public Employee(string name = "<No Name>", string title = "<No Title>")
        {
            this.Name = name;
            this.Title = title;
        }
    }

    var jack = new Employee("Jack", "Associate");   //bad practice in this line
O código acima compila e funciona bem, até que o construtor seja alterado algum dia como:

    //Evil Code: add optional parameters between existing optional parameters
    public Employee(string name = "<No Name>", string department = "intern", string title = "<No Title>")
    {
        this.Name = name;
        this.Department = department;
        this.Title = title;
    }
   
    //the below code still compiles, but now "Associate" is an argument of "department"
    var jack = new Employee("Jack", "Associate");

Prática recomendada para evitar bugs quando "alguém da equipe" cometeu erros:

    var jack = new Employee(name: "Jack", title: "Associate");



