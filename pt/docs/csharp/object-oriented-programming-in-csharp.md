---
title: "Programação Orientada a Objetos em C#"
slug: "programacao-orientada-a-objetos-em-c"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Este tópico tenta nos dizer como podemos escrever programas baseados na abordagem OOP. Mas não tentamos ensinar o paradigma da Programação Orientada a Objetos.
Estaremos abordando os seguintes tópicos:
Classes, Propriedades, Herança, Polimorfismo, Interfaces e assim por diante.

## Aulas:
O esqueleto da classe declarante é:

<>:Obrigatório

[]:Opcional

    [private/public/protected/internal] class <Desired Class Name> [:[Inherited class][,][[Interface Name 1],[Interface Name 2],...]
    {
        //Your code
    }
Não se preocupe se você não consegue entender toda a sintaxe, nós vamos nos familiarizar com todas as partes disso. para o primeiro exemplo, considere a seguinte classe:

    class MyClass
    {
        int i = 100;
        public void getMyValue()
        {
            Console.WriteLine(this.i);//Will print number 100 in output
        }
    }

nesta classe criamos a variável `i` com o tipo `int` e com o método privado padrão [Access Modifiers](https://msdn.microsoft.com/en-us/library/ms173121.aspx) e `getMyValue()` com modificadores de acesso público.

