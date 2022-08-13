---
title: "Tipo dinâmico"
slug: "tipo-dinamico"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

A palavra-chave `dynamic` declara uma variável cujo tipo não é conhecido em tempo de compilação. Uma variável `dinâmica` pode conter qualquer valor, e o tipo do valor pode mudar durante o tempo de execução.

Conforme observado no livro "Metaprogramação em .NET", C# não possui um tipo de suporte para a palavra-chave `dynamic`:

> A funcionalidade habilitada pela palavra-chave `dynamic` é um conjunto inteligente de ações do compilador que emitem e usam objetos `CallSite` no contêiner do site do escopo de execução local. O compilador gerencia o que os programadores percebem como objeto dinâmico
referências através dessas instâncias `CallSite`. Os parâmetros, tipos de retorno, campos e propriedades que recebem tratamento dinâmico em tempo de compilação podem ser marcados com alguns metadados para indicar que foram gerados para uso dinâmico, mas o tipo de dados subjacente para eles sempre será `System.Object`.

 

## Criando um objeto dinâmico com propriedades
    using System;
    using System.Dynamic;
    
    dynamic info = new ExpandoObject();
    info.Id = 123;
    info.Another = 456;
    
    Console.WriteLine(info.Another);
    // 456
    
    Console.WriteLine(info.DoesntExist);
    // Throws RuntimeBinderException

## Criando uma variável dinâmica
    dynamic foo = 123;
    Console.WriteLine(foo + 234);
    // 357    Console.WriteLine(foo.ToUpper())
    // RuntimeBinderException, since int doesn't have a ToUpper method

    foo = "123";
    Console.WriteLine(foo + 234);
    // 123234
    Console.WriteLine(foo.ToUpper()):
    // NOW A STRING

## Dinâmica de retorno
    using System;

    public static void Main()
    {
        var value = GetValue();
        Console.WriteLine(value);
        // dynamics are useful!
    }
    
    private static dynamic GetValue()
    {
        return "dynamics are useful!";
    }

## Manipulando Tipos Específicos Desconhecidos em Tempo de Compilação


