---
title: "Transbordar"
slug: "transbordar"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Estouro de inteiro
Existe uma capacidade máxima que um número inteiro pode armazenar. E quando você ultrapassar esse limite, ele voltará para o lado negativo. Para `int`, é `2147483647`

    int x = int.MaxValue;                //MaxValue is 2147483647
    x = unchecked(x + 1);                //make operation explicitly unchecked so that the example also works when the check for arithmetic overflow/underflow is enabled in the project settings 
    Console.WriteLine(x);                //Will print -2147483648
    Console.WriteLine(int.MinValue);     //Same as Min value

Para quaisquer inteiros fora deste intervalo, use o namespace System.Numerics que possui tipo de dados
BigInteger. Verifique o link abaixo para obter mais informações https://msdn.microsoft.com/en-us/library/system.numerics.biginteger(v=vs.110).aspx

## Estouro durante a operação
O estouro também acontece durante a operação. No exemplo a seguir, x é um `int`, 1 é um `int` por padrão. Portanto, a adição é uma adição `int`. E o resultado será um `int`. E vai transbordar.

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1;                     //It will be overflown
    Console.WriteLine(y);               //Will print -2147483648
    Console.WriteLine(int.MinValue);    //Same as Min value

Você pode evitar isso usando 1L. Agora 1 será uma adição 'longa' e a adição será uma adição 'longa'

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1L;                    //It will be OK
    Console.WriteLine(y);               //Will print 2147483648


## Questões de pedidos
Há estouro no código a seguir

    int x = int.MaxValue;
    Console.WriteLine(x + x + 1L);  //prints -1

Considerando que no código a seguir não há estouro

    int x = int.MaxValue;
    Console.WriteLine(x + 1L + x);  //prints 4294967295

Isso se deve à ordenação das operações da esquerda para a direita. No primeiro fragmento de código `x + x` estoura e depois disso se torna um `long`. Por outro lado `x + 1L` se torna `long` e depois disso `x` é adicionado a este valor.


