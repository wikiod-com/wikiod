---
title: "Pour chaque"
slug: "pour-chaque"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

** L'utiliser du tout ? **

Vous pourriez dire que l'intention du framework .NET est que les requêtes n'ont pas d'effets secondaires et que la méthode "ForEach" provoque par définition un effet secondaire. Vous pourriez trouver votre code plus maintenable et plus facile à tester si vous utilisez plutôt un simple "foreach".


## Méthode d'extension pour IEnumerable
`ForEach()` est défini sur la classe `List<T>`, mais pas sur `IQueryable<T>` ou `IEnumerable<T>`. Vous avez deux choix dans ces cas :

**ToList en premier**

L'énumération (ou la requête) sera évaluée, en copiant les résultats dans une nouvelle liste ou en appelant la base de données. La méthode est ensuite appelée sur chaque élément.
    
    IEnumerable<Customer> customers = new List<Customer>();
    
    customers.ToList().ForEach(c => c.SendEmail());
    
Cette méthode a une surcharge d'utilisation de la mémoire évidente, car une liste intermédiaire est créée.

**Méthode d'extension**

Écrivez une méthode d'extension :

    public static void ForEach<T>(this IEnumerable<T> enumeration, Action<T> action)
    {
        foreach(T item in enumeration)
        {
            action(item);
        }
    }

Utilisation:

    IEnumerable<Customer> customers = new List<Customer>();

    customers.ForEach(c => c.SendEmail());
    
Attention : Les méthodes LINQ du Framework ont ​​été conçues avec l'intention d'être *pures*, ce qui signifie qu'elles ne produisent pas d'effets secondaires. Le seul but de la méthode `ForEach` est de produire des effets secondaires et s'écarte des autres méthodes dans cet aspect. Vous pouvez envisager d'utiliser simplement une boucle "foreach" à la place.

## Appel d'une méthode sur un objet dans une liste
    public class Customer {
       public void SendEmail()
       {
           // Sending email code here
       }
    }
    
    List<Customer> customers = new List<Customer>();
    
    customers.Add(new Customer());
    customers.Add(new Customer());

    customers.ForEach(c => c.SendEmail());

