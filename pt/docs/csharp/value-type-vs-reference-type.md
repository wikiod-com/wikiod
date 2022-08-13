---
title: "Tipo de valor x tipo de referência"
slug: "tipo-de-valor-x-tipo-de-referencia"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Sintaxe
* Passando por referência: public void Double(ref int numberToDouble) { }

## Introdução

### Tipos de valor

Os tipos de valor são os mais simples dos dois. Os tipos de valor são frequentemente usados ​​para representar os próprios dados. Um inteiro, um booleano ou um ponto no espaço 3D são exemplos de bons tipos de valor.

Os tipos de valor (structs) são declarados usando a palavra-chave struct. Consulte a seção de sintaxe para obter um exemplo de como declarar uma nova estrutura.

De um modo geral, temos 2 palavras-chave que são usadas para declarar tipos de valor:

- Estruturas
- Enumerações

### Tipos de referência

Os tipos de referência são um pouco mais complexos. Tipos de referência são objetos tradicionais no sentido de Programação Orientada a Objetos. Então, eles suportam herança (e os benefícios dela) e também suportam finalizadores.

Em C# geralmente temos esses tipos de referência:
- Aulas
- Delegados
- Interfaces

Novos tipos de referência (classes) são declarados usando a palavra-chave class. Para obter um exemplo, consulte a seção de sintaxe para saber como declarar um novo tipo de referência.

## Principais diferenças

As principais diferenças entre os tipos de referência e os tipos de valor podem ser vistas abaixo.

### Os tipos de valor existem na pilha, os tipos de referência existem no heap

Esta é a diferença frequentemente mencionada entre os dois, mas na verdade, o que se resume é que quando você usa um tipo de valor em C#, como um int, o programa usará essa variável para se referir diretamente a esse valor. Se você diz int mine = 0, então a variável mine se refere diretamente a 0, o que é eficiente. No entanto, os tipos de referência realmente mantêm (como o nome sugere) uma referência ao objeto subjacente, isso é semelhante a ponteiros em outras linguagens, como C++.

Você pode não notar os efeitos disso imediatamente, mas os efeitos estão lá, são poderosos e sutis. Veja o exemplo sobre como alterar os tipos de referência em outro lugar para obter um exemplo.

Essa diferença é a principal razão para as outras diferenças a seguir e vale a pena saber.

### Os tipos de valor não mudam quando você os altera em um método, os tipos de referência sim

Quando um tipo de valor é passado para um método como um parâmetro, se o método altera o valor de alguma forma, o valor não é alterado. outras coisas que usam esse mesmo objeto terão o objeto recém-alterado em vez de seu valor original.

Veja o exemplo de tipos de valor versus tipos de referência em métodos para obter mais informações.

#### E se eu quiser alterá-los?

Basta passá-los para o seu método usando a palavra-chave "ref" e você passará esse objeto por referência. Ou seja, é o mesmo objeto na memória. Assim, as modificações que você fizer serão respeitadas. Veja o exemplo de passagem por referência para um exemplo.

### Os tipos de valor não podem ser nulos, os tipos de referência podem

Praticamente como diz, você pode atribuir null a um tipo de referência, o que significa que a variável que você atribuiu não pode ter nenhum objeto real atribuído a ela. No caso de tipos de valor, no entanto, isso não é possível. Você pode, no entanto, usar Nullable<Type>, para permitir que seu tipo de valor seja anulável, se isso for um requisito, mas se isso for algo que você está considerando, pense fortemente se uma classe pode não ser a melhor abordagem aqui, se é o seu próprio tipo.

## Passando por referência usando a palavra-chave ref.

Da [documentação][1]:


> Em C#, argumentos podem ser passados ​​para parâmetros por valor ou por
> referência. A passagem por referência habilita membros de função, métodos,
> propriedades, indexadores, operadores e construtores para alterar o valor
> dos parâmetros e fazer com que essa alteração persista na chamada
> meio ambiente. Para passar um parâmetro por referência, use o `ref` ou `out`
> palavra-chave.

A diferença entre `ref` e `out` é que `out` significa que o parâmetro passado deve ser atribuído antes que a função termine. Em contraste, os parâmetros passados ​​com `ref` podem ser alterados ou deixados inalterados.


    using System;

    class Program
    {
        static void Main(string[] args)
        {
            int a = 20;
            Console.WriteLine("Inside Main - Before Callee: a = {0}", a);
            Callee(a);
            Console.WriteLine("Inside Main - After Callee: a = {0}", a);
            
            Console.WriteLine("Inside Main - Before CalleeRef: a = {0}", a);
            CalleeRef(ref a);
            Console.WriteLine("Inside Main - After CalleeRef: a = {0}", a);
         
            Console.WriteLine("Inside Main - Before CalleeOut: a = {0}", a);
            CalleeOut(out a);
            Console.WriteLine("Inside Main - After CalleeOut: a = {0}", a);
            
            Console.ReadLine();
        }
    
        static void Callee(int a)
        {
            a = 5;
            Console.WriteLine("Inside Callee a : {0}", a);
        }
    
        static void CalleeRef(ref int a)
        {
            a = 6;
            Console.WriteLine("Inside CalleeRef a : {0}", a);
        }
        
        static void CalleeOut(out int a)
        {
            a = 7;
            Console.WriteLine("Inside CalleeOut a : {0}", a);
        }
    }

**Resultado** :

    Inside Main - Before Callee: a = 20
    Inside Callee a : 5
    Inside Main - After Callee: a = 20
    Inside Main - Before CalleeRef: a = 20
    Inside CalleeRef a : 6
    Inside Main - After CalleeRef: a = 6
    Inside Main - Before CalleeOut: a = 6
    Inside CalleeOut a : 7
    Inside Main - After CalleeOut: a = 7

[1]: https://msdn.microsoft.com/en-IN/library/0f66670z.aspx


## Alterando valores em outro lugar
<!-- language-all: c# -->
```
public static void Main(string[] args)
{
    var studentList = new List<Student>();
    studentList.Add(new Student("Scott", "Nuke"));
    studentList.Add(new Student("Vincent", "King"));
    studentList.Add(new Student("Craig", "Bertt"));

    // make a separate list to print out later
    var printingList = studentList; // this is a new list object, but holding the same student objects inside it

    // oops, we've noticed typos in the names, so we fix those
    studentList[0].LastName = "Duke";
    studentList[1].LastName = "Kong";
    studentList[2].LastName = "Brett";

    // okay, we now print the list
    PrintPrintingList(printingList);
}

private static void PrintPrintingList(List<Student> students)
{
    foreach (Student student in students)
    {
        Console.WriteLine(string.Format("{0} {1}", student.FirstName, student.LastName));
    }
}
```

Você notará que, embora a lista printingList tenha sido feita antes das correções nos nomes dos alunos após os erros de digitação, o método PrintPrintingList ainda imprime os nomes corrigidos:

    Scott Duke
    Vincent Kong
    Craig Brett

Isso ocorre porque ambas as listas contêm uma lista de referências aos mesmos alunos. Assim, a alteração do objeto aluno subjacente se propaga para usos por qualquer uma das listas.

Veja como seria a classe de alunos.

```
public class Student
{
    public string FirstName { get; set; }
    public string LastName { get; set; }

    public Student(string firstName, string lastName)
    {
        this.FirstName = firstName;
        this.LastName = lastName;
    }
}
```


## Passando por referência
Se você quiser que o exemplo de Tipos de Valor versus Tipos de Referência nos métodos funcione corretamente, use a palavra-chave ref na assinatura do método para o parâmetro que deseja passar por referência, bem como quando chamar o método.
<!-- language-all: c# -->

```
public static void Main(string[] args)
{
    ...
    DoubleNumber(ref number); // calling code
    Console.WriteLine(number); // outputs 8
    ...
}
```

```
public void DoubleNumber(ref int number)
{
    number += number;
}
```

Fazer essas alterações faria com que o número fosse atualizado conforme o esperado, o que significa que a saída do console para o número seria 8.

## Atribuição


## Diferença com os parâmetros do método ref e out
Existem duas maneiras possíveis de passar um tipo de valor por referência: `ref` e `out`. A diferença é que ao passá-lo com `ref` o valor deve ser inicializado, mas não ao passá-lo com `out`. Usar `out` garante que a variável tenha um valor após a chamada do método:

    public void ByRef(ref int value)
    {
        Console.WriteLine(nameof(ByRef) + value);
        value += 4;
        Console.WriteLine(nameof(ByRef) + value);
    }

    public void ByOut(out int value)
    {
        value += 4 // CS0269: Use of unassigned out parameter `value'  
        Console.WriteLine(nameof(ByOut) + value); // CS0269: Use of unassigned out parameter `value'  

        value = 4;
        Console.WriteLine(nameof(ByOut) + value);
    }

    public void TestOut()
    {
        int outValue1;
        ByOut(out outValue1); // prints 4

        int outValue2 = 10;   // does not make any sense for out
        ByOut(out outValue2); // prints 4
    }

    public void TestRef()
    {
        int refValue1;
        ByRef(ref refValue1); // S0165  Use of unassigned local variable 'refValue'

        int refValue2 = 0;
        ByRef(ref refValue2); // prints 0 and 4

        int refValue3 = 10;
        ByRef(ref refValue3); // prints 10 and 14
    }

O problema é que usando `out` o parâmetro `must` deve ser inicializado antes de sair do método, portanto o seguinte método é possível com `ref` mas não com `out`:


    public void EmtyRef(bool condition, ref int value)
    {
        if (condition)
        {
            value += 10;
        }
    }

    public void EmtyOut(bool condition, out int value)
    {
        if (condition)
        {
            value = 10;
        }
    } //CS0177: The out parameter 'value' must be assigned before control leaves the current method

Isso ocorre porque se a `condição` não for válida, o `valor` não será atribuído.

## parâmetros de referência vs saída


