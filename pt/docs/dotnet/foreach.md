---
title: "Para cada"
slug: "para-cada"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

**Usá-lo em tudo?**

Você pode argumentar que a intenção do framework .NET é que as consultas não tenham nenhum efeito colateral e o método `ForEach` está, por definição, causando um efeito colateral. Você pode achar seu código mais sustentável e mais fácil de testar se usar um `foreach` simples.


## Método de extensão para IEnumerable
`ForEach()` é definido na classe `List<T>`, mas não em `IQueryable<T>` ou `IEnumerable<T>`. Você tem duas opções nesses casos:

**ToList primeiro**

A enumeração (ou consulta) será avaliada, copiando os resultados em uma nova lista ou chamando o banco de dados. O método é então chamado em cada item.
    
    IEnumerable<Customer> customers = new List<Customer>();
    
    customers.ToList().ForEach(c => c.SendEmail());
    
Esse método tem uma sobrecarga de uso de memória óbvia, pois uma lista intermediária é criada.

**Método de extensão**

Escreva um método de extensão:

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
    
Cuidado: Os métodos LINQ do Framework foram projetados com a intenção de serem *puros*, o que significa que não produzem efeitos colaterais. O único propósito do método `ForEach` é produzir efeitos colaterais, e se desvia dos outros métodos neste aspecto. Você pode considerar apenas usar um loop `foreach` simples.

## Chamando um método em um objeto em uma lista
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

