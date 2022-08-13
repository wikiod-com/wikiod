---
title: "Ponteiros"
slug: "ponteiros"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

# Ponteiros e `inseguro`

Devido à sua natureza, os ponteiros produzem código não verificável. Assim, o uso de qualquer tipo de ponteiro requer um contexto `inseguro`.

O tipo `System.IntPtr` é um wrapper seguro em torno de um `void*`. Destina-se a ser uma alternativa mais conveniente para `void*` quando um contexto inseguro não é necessário para executar a tarefa em questão.

# Comportamento indefinido

Como em C e C++, o uso incorreto de ponteiros pode invocar um comportamento indefinido, com possíveis efeitos colaterais como corrupção de memória e execução de código não intencional. Devido à natureza não verificável da maioria das operações de ponteiro, o uso correto de ponteiros é de inteira responsabilidade do programador.

# Tipos que suportam ponteiros

Ao contrário de C e C++, nem todos os tipos de C# têm tipos de ponteiro correspondentes. Um tipo `T` pode ter um tipo de ponteiro correspondente se ambos os critérios a seguir se aplicarem:

- `T` é um tipo de estrutura ou um tipo de ponteiro.
- `T` contém apenas membros que satisfazem ambos os critérios recursivamente.

## Ponteiros para acesso ao array
Este exemplo demonstra como os ponteiros podem ser usados ​​para acesso semelhante a C a matrizes C#.

    unsafe
    {
        var buffer = new int[1024];
        fixed (int* p = &buffer[0])
        {
            for (var i = 0; i < buffer.Length; i++)
            {
                *(p + i) = i;
            }
        }
    }

A palavra-chave `unsafe` é necessária porque o acesso do ponteiro não emitirá verificações de limites que normalmente são emitidas ao acessar matrizes C# da maneira regular.

A palavra-chave `fixed` diz ao compilador C# para emitir instruções para fixar o objeto de forma segura para exceção. A fixação é necessária para garantir que o coletor de lixo não mova a matriz na memória, pois isso invalidaria quaisquer ponteiros apontando para a matriz.

## Aritmética de ponteiro
Adição e subtração em ponteiros funcionam de maneira diferente dos inteiros. Quando um ponteiro é incrementado ou decrementado, o endereço para o qual ele aponta é aumentado ou diminuído pelo tamanho do tipo de referência.

Por exemplo, o tipo `int` (alias para `System.Int32`) tem um tamanho de 4. Se um `int` pode ser armazenado no endereço 0, o `int` subsequente pode ser armazenado no endereço 4, e assim por diante . Em código:

    var ptr = (int*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 4
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8

Da mesma forma, o tipo `long` (alias para `System.Int64`) tem um tamanho de 8. Se um `long` pode ser armazenado no endereço 0, o `long` subseqüente pode ser armazenado no endereço 8, e assim por diante. Em código:

    var ptr = (long*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 16

O tipo `void` é especial e os ponteiros `void` também são especiais e são usados ​​como ponteiros catch-all quando o tipo não é conhecido ou não importa. Devido à sua natureza independente de tamanho, os ponteiros `void` não podem ser incrementados ou decrementados:

    var ptr = (void*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));

## O asterisco faz parte do tipo
Em C e C++, o asterisco na declaração de uma variável de ponteiro é *parte da expressão* que está sendo declarada. Em C#, o asterisco na declaração é *parte do tipo*.

Em C, C++ e C#, o trecho a seguir declara um ponteiro `int`:

    int* a;

Em C e C++, o trecho a seguir declara um ponteiro `int` e uma variável `int`. Em C#, ele declara dois ponteiros `int`:

    int* a, b; 

Em C e C++, o trecho a seguir declara dois ponteiros `int`. Em C#, é inválido:

    int *a, *b;

## vazio*
C# herda de C e C++ o uso de `void*` como um ponteiro independente de tipo e tamanho.

    void* ptr;

Qualquer tipo de ponteiro pode ser atribuído a `void*` usando uma conversão implícita:

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;

O inverso requer uma conversão explícita:

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;
    int* p2 = (int*)ptr;

## Acesso de membro usando ->
C# herda de C e C++ o uso do símbolo `->` como meio de acessar os membros de uma instância através de um ponteiro tipado.

Considere a seguinte estrutura:

    struct Vector2
    {
        public int X;
        public int Y;
    }

Este é um exemplo do uso de `->` para acessar seus membros:

    Vector2 v;
    v.X = 5;
    v.Y = 10;

    Vector2* ptr = &v;
    int x = ptr->X;
    int y = ptr->Y;
    string s = ptr->ToString();

    Console.WriteLine(x); // prints 5
    Console.WriteLine(y); // prints 10
    Console.WriteLine(s); // prints Vector2

## Ponteiros genéricos
Os critérios que um tipo deve satisfazer para oferecer suporte a ponteiros (consulte *Comentários*) não podem ser expressos em termos de restrições genéricas. Portanto, qualquer tentativa de declarar um ponteiro para um tipo fornecido por meio de um parâmetro de tipo genérico falhará.

    void P<T>(T obj) 
        where T : struct
    {
        T* ptr = &obj; // compile-time error
    }


