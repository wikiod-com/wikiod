---
title: "Argumentos con nombre"
slug: "argumentos-con-nombre"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## El orden de los argumentos no es necesario
Puede colocar argumentos con nombre en el orden que desee.

Método de muestra:

    public static string Sample(string left, string right)
    {
         return string.Join("-",left,right);
    }

Ejemplo de llamada:

    Console.WriteLine (Sample(left:"A",right:"B"));
    Console.WriteLine (Sample(right:"A",left:"B"));

Resultados:

    A-B
    B-A
    


## Los argumentos con nombre pueden hacer que su código sea más claro
Considere esta clase simple:

    class SmsUtil
    {
        public bool SendMessage(string from, string to, string message, int retryCount, object attachment)
        {
             // Some code
        }
    }

Antes de C# 3.0 era:

    var result = SmsUtil.SendMessage("Mehran", "Maryam", "Hello there!", 12, null);

puede hacer que esta llamada de método sea aún más clara con **argumentos con nombre**:

    var result = SmsUtil.SendMessage(
        from: "Mehran",
        to:  "Maryam",
        message "Hello there!",
        retryCount: 12,
        attachment: null);


## Argumentos con nombre y parámetros opcionales
Puede combinar argumentos con nombre con parámetros opcionales.

Veamos este método:

    
    public sealed class SmsUtil
    {
        public static bool SendMessage(string from, string to, string message, int retryCount = 5, object attachment = null)
        {
             // Some code
        }
    }

Cuando desee llamar a este método *sin* establezca el argumento `retryCount`:


    var result = SmsUtil.SendMessage(
                            from       : "Cihan",
                            to         : "Yakar",
                            message    : "Hello there!",
                            attachment : new object());

## Argumentos con nombre evita errores en parámetros opcionales
Siempre use argumentos con nombre para parámetros opcionales, para evitar posibles errores cuando se modifica el método.

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
El código anterior se compila y funciona bien, hasta que el constructor se cambia algún día como:

    //Evil Code: add optional parameters between existing optional parameters
    public Employee(string name = "<No Name>", string department = "intern", string title = "<No Title>")
    {
        this.Name = name;
        this.Department = department;
        this.Title = title;
    }
   
    //the below code still compiles, but now "Associate" is an argument of "department"
    var jack = new Employee("Jack", "Associate");

Mejores prácticas para evitar errores cuando "alguien más en el equipo" cometió errores:

    var jack = new Employee(name: "Jack", title: "Associate");



