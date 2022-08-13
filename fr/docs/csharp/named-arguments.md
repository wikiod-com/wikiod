---
title: "Arguments nommés"
slug: "arguments-nommes"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## L'ordre des arguments n'est pas nécessaire
Vous pouvez placer des arguments nommés dans l'ordre de votre choix.

Exemple de méthode :

    public static string Sample(string left, string right)
    {
         return string.Join("-",left,right);
    }

Exemple d'appel :

    Console.WriteLine (Sample(left:"A",right:"B"));
    Console.WriteLine (Sample(right:"A",left:"B"));

Résultats:

    A-B
    B-A
    


## Les arguments nommés peuvent rendre votre code plus clair
Considérez cette classe simple :

    class SmsUtil
    {
        public bool SendMessage(string from, string to, string message, int retryCount, object attachment)
        {
             // Some code
        }
    }

Avant C# 3.0, c'était :

    var result = SmsUtil.SendMessage("Mehran", "Maryam", "Hello there!", 12, null);

vous pouvez rendre cet appel de méthode encore plus clair avec des **arguments nommés** :

    var result = SmsUtil.SendMessage(
        from: "Mehran",
        to:  "Maryam",
        message "Hello there!",
        retryCount: 12,
        attachment: null);


## Arguments nommés et paramètres optionnels
Vous pouvez combiner des arguments nommés avec des paramètres facultatifs.

Voyons cette méthode :

    
    public sealed class SmsUtil
    {
        public static bool SendMessage(string from, string to, string message, int retryCount = 5, object attachment = null)
        {
             // Some code
        }
    }

Lorsque vous voulez appeler cette méthode *sans* définir l'argument `retryCount` :


    var result = SmsUtil.SendMessage(
                            from       : "Cihan",
                            to         : "Yakar",
                            message    : "Hello there!",
                            attachment : new object());

## Les arguments nommés évitent les bugs sur les paramètres optionnels
Utilisez toujours des arguments nommés pour les paramètres facultatifs, afin d'éviter les bogues potentiels lorsque la méthode est modifiée.

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
Le code ci-dessus se compile et fonctionne correctement, jusqu'à ce que le constructeur soit modifié un jour comme :

    //Evil Code: add optional parameters between existing optional parameters
    public Employee(string name = "<No Name>", string department = "intern", string title = "<No Title>")
    {
        this.Name = name;
        this.Department = department;
        this.Title = title;
    }
   
    //the below code still compiles, but now "Associate" is an argument of "department"
    var jack = new Employee("Jack", "Associate");

Meilleure pratique pour éviter les bugs lorsque "quelqu'un d'autre dans l'équipe" fait des erreurs :

    var jack = new Employee(name: "Jack", title: "Associate");



