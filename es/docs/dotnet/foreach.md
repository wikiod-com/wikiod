---
title: "Para cada"
slug: "para-cada"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

** ¿Usarlo en absoluto? **

Podría argumentar que la intención del marco .NET es que las consultas no tengan efectos secundarios y que el método `ForEach`, por definición, causa un efecto secundario. Es posible que encuentre su código más fácil de mantener y más fácil de probar si usa un 'foreach' simple en su lugar.


## Método de extensión para IEnumerable
`ForEach()` está definido en la clase `List<T>`, pero no en `IQueryable<T>` o `IEnumerable<T>`. Tienes dos opciones en esos casos:

**ToList primero**

Se evaluará la enumeración (o consulta), copiando los resultados en una nueva lista o llamando a la base de datos. A continuación, se llama al método en cada elemento.
    
    IEnumerable<Customer> customers = new List<Customer>();
    
    customers.ToList().ForEach(c => c.SendEmail());
    
Este método tiene una sobrecarga de uso de memoria obvia, ya que se crea una lista intermedia.

**Método de extensión**

Escriba un método de extensión:

    public static void ForEach<T>(this IEnumerable<T> enumeration, Action<T> action)
    {
        foreach(T item in enumeration)
        {
            action(item);
        }
    }

Usar:

    IEnumerable<Customer> customers = new List<Customer>();

    customers.ForEach(c => c.SendEmail());
    
Precaución: Los métodos LINQ de Framework se han diseñado con la intención de ser *puros*, lo que significa que no producen efectos secundarios. El único propósito del método `ForEach` es producir efectos secundarios y se desvía de los otros métodos en este aspecto. En su lugar, puede considerar simplemente usar un ciclo `foreach` simple.

## Llamar a un método en un objeto en una lista
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

