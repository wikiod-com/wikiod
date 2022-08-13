---
title: "Ponteiros e código inseguro"
slug: "ponteiros-e-codigo-inseguro"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Introdução ao código inseguro
C# permite usar variáveis ​​de ponteiro em uma função de bloco de código quando marcada pelo modificador `unsafe`. O código não seguro ou o código não gerenciado é um bloco de código que usa uma variável de ponteiro.

Um ponteiro é uma variável cujo valor é o endereço de outra variável, ou seja, o endereço direto da localização da memória. semelhante a qualquer variável ou constante, você deve declarar um ponteiro antes de poder usá-lo para armazenar qualquer endereço de variável.

A forma geral de uma declaração de ponteiro é:

    type *var-name;

A seguir estão as declarações de ponteiro válidas:

    int    *ip;    /* pointer to an integer */
    double *dp;    /* pointer to a double */
    float  *fp;    /* pointer to a float */
    char   *ch     /* pointer to a character */
O exemplo a seguir ilustra o uso de ponteiros em C#, usando o modificador unsafe:

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          static unsafe void Main(string[] args)
          {
             int var = 20;
             int* p = &var;
             Console.WriteLine("Data is: {0} ",  var);
             Console.WriteLine("Address is: {0}",  (int)p);
             Console.ReadKey();
          }
       }
    }
Quando o código acima foi compilado e executado, ele produz o seguinte resultado:

    Data is: 20
    Address is: 99215364

Em vez de declarar um método inteiro como inseguro, você também pode declarar uma parte do código como insegura:

    // safe code
    unsafe
    {
        // you can use pointers here
    }
    // safe code

## Acessando elementos do array usando um ponteiro
Em C#, um nome de matriz e um ponteiro para um tipo de dados igual aos dados de matriz não são do mesmo tipo de variável. Por exemplo, `int *p` e `int[] p`, não são do mesmo tipo. Você pode incrementar a variável de ponteiro `p` porque ela não é fixa na memória, mas um endereço de matriz é fixo na memória, e você não pode incrementá-la.

Portanto, se você precisar acessar dados de um array usando uma variável de ponteiro, como tradicionalmente fazemos em C ou C++, você precisa corrigir o ponteiro usando a palavra-chave fixed.

O exemplo a seguir demonstra isso:

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe static void Main()
          {
             int[]  list = {10, 100, 200};
             fixed(int *ptr = list)
             
             /* let us have array address in pointer */
             for ( int i = 0; i < 3; i++)
             {
                Console.WriteLine("Address of list[{0}]={1}",i,(int)(ptr + i));
                Console.WriteLine("Value of list[{0}]={1}", i, *(ptr + i));
             }
             
             Console.ReadKey();
          }
       }
    }

Quando o código acima foi compilado e executado, ele produz o seguinte resultado:

    Address of list[0] = 31627168
    Value of list[0] = 10
    Address of list[1] = 31627172
    Value of list[1] = 100
    Address of list[2] = 31627176
    Value of list[2] = 200

## Compilando código inseguro
Para compilar código inseguro, você deve especificar a opção de linha de comando `/unsafe` com o compilador de linha de comando.

Por exemplo, para compilar um programa chamado prog1.cs contendo código não seguro, a partir da linha de comando, dê o comando:

    csc /unsafe prog1.cs

Se você estiver usando o Visual Studio IDE, precisará habilitar o uso de código não seguro nas propriedades do projeto.

[![digite a descrição da imagem aqui][1]][1]

Para fazer isso:

- Abra as propriedades do projeto clicando duas vezes no nó de propriedades na
Explorador de Soluções.
- Clique na guia Construir.
- Selecione a opção "Permitir
código inseguro"


[1]: https://i.stack.imgur.com/2aPFY.png

## Recuperando o valor dos dados usando um ponteiro
Você pode recuperar os dados armazenados no local referenciado pela variável ponteiro, usando o método ToString(). O exemplo a seguir demonstra isso:

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          public static void Main()
          {
             unsafe
             {
                int var = 20;
                int* p = &var;
                Console.WriteLine("Data is: {0} " , var);
                Console.WriteLine("Data is: {0} " , p->ToString());
                Console.WriteLine("Address is: {0} " , (int)p);
             }
             
             Console.ReadKey();
          }
       }
    }
Quando o código acima foi compilado e executado, ele produz o seguinte resultado:

    Data is: 20
    Data is: 20
    Address is: 77128984

## Passando ponteiros como parâmetros para métodos
Você pode passar uma variável de ponteiro para um método como parâmetro. O exemplo a seguir ilustra isso:

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe void swap(int* p, int *q)
          {
             int temp = *p;
             *p = *q;
             *q = temp;
          }
          
          public unsafe static void Main()
          {
             TestPointer p = new TestPointer();
             int var1 = 10;
             int var2 = 20;
             int* x = &var1;
             int* y = &var2;
             
             Console.WriteLine("Before Swap: var1:{0}, var2: {1}", var1, var2);
             p.swap(x, y);
    
             Console.WriteLine("After Swap: var1:{0}, var2: {1}", var1, var2);
             Console.ReadKey();
          }
       }
    }

Quando o código acima é compilado e executado, ele produz o seguinte resultado:

    Before Swap: var1: 10, var2: 20
    After Swap: var1: 20, var2: 10

