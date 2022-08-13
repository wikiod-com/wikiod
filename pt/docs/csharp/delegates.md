---
title: "Delegados"
slug: "delegados"
draft: false
images: []
weight: 9752
type: docs
toc: true
---

# Resumo

Um **tipo de delegado** é um tipo que representa uma assinatura de método específica. Uma instância desse tipo se refere a um método específico com uma assinatura correspondente. Parâmetros de método podem ter tipos delegados e, portanto, este método deve receber uma referência a outro método, que pode ser invocado

# Tipos de delegado embutidos: `Action<...>`, `Predicate<T>` e `Func<...,TResult>`

O namespace `System` contém os delegados `Action<...>`,`Predicate<T>` e `Func<...,TResult>`, onde o "..." representa entre 0 e 16 parâmetros de tipo genérico ( para 0 parâmetros, `Ação` não é genérico).

`Func` representa métodos com um tipo de retorno correspondente a `TResult`, e `Action` representa métodos sem valor de retorno (void). Em ambos os casos, os parâmetros de tipo genérico adicionais correspondem, em ordem, aos parâmetros do método.

`Predicate` representa o método com tipo de retorno booleano, T é o parâmetro de entrada.

# Tipos de delegados personalizados

Os tipos de delegados nomeados podem ser declarados usando a palavra-chave `delegate`.

# Invocando delegados

Os delegados podem ser invocados usando a mesma sintaxe dos métodos: o nome da instância do delegado, seguido por parênteses contendo quaisquer parâmetros.

# Atribuindo a delegados

Os delegados podem ser atribuídos das seguintes maneiras:

- Atribuindo um método nomeado
- Atribuindo um método anônimo usando um lambda
- Atribuindo um método nomeado usando a palavra-chave `delegate`.

# Combinando delegados

Vários objetos delegados podem ser atribuídos a uma instância delegada usando o operador `+`. O operador `-` pode ser usado para remover um delegado de componente de outro delegado.

## Declarando um tipo de delegado
A sintaxe a seguir cria um tipo `delegate` com o nome `NumberInOutDelegate`, representando um método que recebe um `int` e retorna um `int`.

    public delegate int NumberInOutDelegate(int input);

Isso pode ser usado da seguinte forma:

    public static class Program
    {
        static void Main()
        {
            NumberInOutDelegate square = MathDelegates.Square;
            int answer1 = square(4); 
            Console.WriteLine(answer1); // Will output 16

            NumberInOutDelegate cube = MathDelegates.Cube;
            int answer2 = cube(4);
            Console.WriteLine(answer2); // Will output 64            
        }
    }
    
    public static class MathDelegates
    {
        static int Square (int x)
        {
            return x*x;
        }

        static int Cube (int x)
        {
            return x*x*x;
        }
    }

A instância do delegado `example` é executada da mesma forma que o método `Square`. Uma instância de delegado atua literalmente como um delegado para o chamador: o chamador invoca o
delegado e, em seguida, o delegado chama o método de destino. Essa indireção separa
o chamador do método de destino.

----------

Você pode declarar um tipo delegado __generic__ e, nesse caso, pode especificar que o tipo é covariante (`out`) ou contravariante (`in`) em alguns dos argumentos de tipo. Por exemplo:

    public delegate TTo Converter<in TFrom, out TTo>(TFrom input);

Como outros tipos genéricos, os tipos delegados genéricos podem ter restrições, como `where TFrom : struct, IConvertible where TTo : new()`.

Evite co e contravariância para tipos de delegados que devem ser usados ​​para delegados multicast, como tipos de manipuladores de eventos. Isso ocorre porque a concatenação (`+`) pode falhar se o tipo de tempo de execução for diferente do tipo de tempo de compilação devido à variação. Por exemplo, evite:

    public delegate void EventHandler<in TEventArgs>(object sender, TEventArgs e);

Em vez disso, use um tipo genérico invariável:

    public delegate void EventHandler<TEventArgs>(object sender, TEventArgs e);

----------

Também há suporte para delegados onde alguns parâmetros são modificados por `ref` ou `out`, como em:

    public delegate bool TryParser<T>(string input, out T result);

(exemplo use `TryParser<decimal> exemplo = decimal.TryParse;`), ou delegados onde o último parâmetro tem o modificador `params`. Os tipos de delegado podem ter parâmetros opcionais (fornecer valores padrão). Os tipos de delegado podem usar tipos de ponteiro como `int*` ou `char*` em suas assinaturas ou tipos de retorno (use a palavra-chave `unsafe`). Um tipo de delegado e seus parâmetros podem carregar atributos personalizados.

## Os tipos de delegado Func<T, TResult>, Action<T> e Predicate<T>
O namespace System contém tipos de delegado `Func<..., TResult>` com entre 0 e 15 parâmetros genéricos, retornando o tipo `TResult`.

    private void UseFunc(Func<string> func)
    {
        string output = func(); // Func with a single generic type parameter returns that type
        Console.WriteLine(output);
    }

    private void UseFunc(Func<int, int, string> func)
    {
        string output = func(4, 2); // Func with multiple generic type parameters takes all but the first as parameters of that type
        Console.WriteLine(output);
    }

O namespace System também contém tipos de delegado `Action<...>` com diferentes números de parâmetros genéricos (de 0 a 16). É semelhante a `Func<T1, .., Tn>`, mas sempre retorna `void`.

    private void UseAction(Action action)
    {
        action(); // The non-generic Action has no parameters
    }

    private void UseAction(Action<int, string> action)
    {
        action(4, "two"); // The generic action is invoked with parameters matching its type arguments
    }

`Predicate<T>` também é uma forma de `Func`, mas sempre retornará `bool`. Um predicado é uma maneira de especificar um critério personalizado. Dependendo do valor da entrada e da lógica definida no predicado, ele retornará `true` ou `false`. `Predicate<T>`, portanto, se comporta da mesma maneira que `Func<T, bool>` e ambos podem ser inicializados e usados ​​da mesma maneira.
    
    Predicate<string> predicate = s => s.StartsWith("a");
    Func<string, bool> func = s => s.StartsWith("a");

    // Both of these return true
    var predicateReturnsTrue = predicate("abc");
    var funcReturnsTrue = func("abc");

    // Both of these return false
    var predicateReturnsFalse = predicate("xyz");
    var funcReturnsFalse = func("xyz");

A escolha de usar `Predicate<T>` ou `Func<T, bool>` é realmente uma questão de opinião. `Predicate<T>` é provavelmente mais expressivo da intenção do autor, enquanto `Func<T, bool>` provavelmente será familiar para uma proporção maior de desenvolvedores C#.

Além disso, existem alguns casos em que apenas uma das opções está disponível, principalmente ao interagir com outra API. Por exemplo, `List<T>` e `Array<T>` geralmente usam `Predicate<T>` para seus métodos, enquanto a maioria das extensões LINQ aceitam apenas `Func<T, bool>`.

## Combinar Delegados (Delegados Multicast)
As operações de adição `+` e subtração `-` podem ser usadas para combinar instâncias delegadas. O delegado contém uma lista dos delegados atribuídos.

    using System;
    using System.Reflection;
    using System.Reflection.Emit;

    namespace DelegatesExample {
        class MainClass {
            private delegate void MyDelegate(int a);

            private static void PrintInt(int a) {
                Console.WriteLine(a);
            }

            private static void PrintType<T>(T a) {
                Console.WriteLine(a.GetType());
            }

            public static void Main (string[] args)
            {
                MyDelegate d1 = PrintInt;
                MyDelegate d2 = PrintType;

                // Output:
                // 1
                d1(1);

                // Output:
                // System.Int32
                d2(1);

                MyDelegate d3 = d1 + d2;
                // Output:
                // 1
                // System.Int32
                d3(1);

                MyDelegate d4 = d3 - d2;
                // Output:
                // 1
                d4(1);

                // Output:
                // True
                Console.WriteLine(d1 == d4);
            }
        }
    }

Neste exemplo, `d3` é uma combinação de delegados `d1` e `d2`, portanto, quando chamado, o programa gera strings `1` e `System.Int32`.


----------

Combinando delegados com tipos de retorno **non void**:

Se um delegado multicast tem um tipo de retorno `nonvoid`, o chamador recebe o valor de retorno
do último método a ser invocado. Os métodos anteriores ainda são chamados, mas seus
os valores de retorno são descartados.

        class Program
        {
            public delegate int Transformer(int x);

            static void Main(string[] args)
            {
                Transformer t = Square;
                t += Cube;
                Console.WriteLine(t(2));  // O/P 8 
            }

            static int Square(int x) { return x * x; }

            static int Cube(int x) { return x*x*x; }
        }

`t(2)` chamará primeiro `Square` e depois `Cube`. O valor de retorno de Square é descartado e o valor de retorno do último método, ou seja, `Cube` é retido.

## Invocação segura do delegado multicast
Sempre quis chamar um delegado multicast, mas deseja que toda a lista de invocação seja chamada, mesmo que ocorra uma exceção em qualquer uma da cadeia. Então você está com sorte, eu criei um método de extensão que faz exatamente isso, lançando um `AggregateException` somente após a execução de toda a lista ser concluída:

    public static class DelegateExtensions
    {
        public static void SafeInvoke(this Delegate del,params object[] args)
        {
            var exceptions = new List<Exception>();

            foreach (var handler in del.GetInvocationList())
            {
                try
                {
                    handler.Method.Invoke(handler.Target, args);
                }
                catch (Exception ex)
                {
                    exceptions.Add(ex);
                }
            }

            if(exceptions.Any())
            {
                throw new AggregateException(exceptions);
            }
        }
    }

    public class Test
    {
        public delegate void SampleDelegate();

        public void Run()
        {
            SampleDelegate delegateInstance = this.Target2;
            delegateInstance += this.Target1;

            try
            {
                delegateInstance.SafeInvoke();
            } 
            catch(AggregateException ex)
            {
                // Do any exception handling here
            }
        }

        private void Target1()
        {
            Console.WriteLine("Target 1 executed");
        }

        private void Target2()
        {
            Console.WriteLine("Target 2 executed");
            throw new Exception();
        }
    }

Isso produz:

    Target 2 executed
    Target 1 executed

Invocar diretamente, sem `SaveInvoke`, só executaria o Target 2.

## Delegar Igualdade
Chamar `.Equals()` em um delegado compara por igualdade de referência:

    Action action1 = () => Console.WriteLine("Hello delegates");
    Action action2 = () => Console.WriteLine("Hello delegates");
    Action action1Again = action1;

    Console.WriteLine(action1.Equals(action1)) // True
    Console.WriteLine(action1.Equals(action2)) // False
    Console.WriteLine(action1Again.Equals(action1)) // True

Essas regras também se aplicam ao fazer `+=` ou `-=` em um delegado multicast, por exemplo, ao assinar e cancelar a assinatura de eventos.

## Referências subjacentes de delegados de métodos nomeados
Ao atribuir métodos nomeados a delegados, eles farão referência ao mesmo objeto subjacente se:

- Eles são o mesmo método de instância, na mesma instância de uma classe
- Eles são o mesmo método estático em uma classe

       public class Greeter
       {
           public void WriteInstance()
           {
               Console.WriteLine("Instance");
           }

           public static void WriteStatic()
           {
               Console.WriteLine("Static");
           }
       }

       // ...

       Greeter greeter1 = new Greeter();
       Greeter greeter2 = new Greeter();

       Action instance1 = greeter1.WriteInstance;
       Action instance2 = greeter2.WriteInstance;
       Action instance1Again = greeter1.WriteInstance;
    
       Console.WriteLine(instance1.Equals(instance2)); // False
       Console.WriteLine(instance1.Equals(instance1Again)); // True

       Action @static = Greeter.WriteStatic;
       Action staticAgain = Greeter.WriteStatic;

       Console.WriteLine(@static.Equals(staticAgain)); // True

## Atribuindo um método nomeado a um delegado
Os métodos nomeados podem ser atribuídos a delegados com assinaturas correspondentes:

    public static class Example
    {
        public static int AddOne(int input)
        {
            return input + 1;
        }
    }


    Func<int,int> addOne = Example.AddOne

`Example.AddOne` recebe um `int` e retorna um `int`, sua assinatura corresponde ao delegado `Func<int,int>`. `Example.AddOne` pode ser atribuído diretamente a `addOne` porque eles têm assinaturas correspondentes.

## Atribuindo a um delegado por lambda
Lambdas pode ser usado para criar métodos anônimos para atribuir a um delegado:

    Func<int,int> addOne = x => x+1;

Observe que a declaração explícita do tipo é necessária ao criar uma variável desta forma:

    var addOne = x => x+1; // Does not work

## Passando delegados como parâmetros
Os delegados podem ser usados ​​como ponteiros de função digitados:

    class FuncAsParameters
    {
      public void Run()
      {
        DoSomething(ErrorHandler1);
        DoSomething(ErrorHandler2);
      }
    
      public bool ErrorHandler1(string message)
      {
        Console.WriteLine(message);
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public bool ErrorHandler2(string message)
      {
        // ...Write message to file...
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public void DoSomething(Func<string, bool> errorHandler)
      {
        // In here, we don't care what handler we got passed!
        ...
        if (...error...)
        {
          if (!errorHandler("Some error occurred!"))
          {
            // The handler decided we can't continue
            return;
          }
        }
      }
    }

## Fechamento dentro de um delegado


## Encapsulando transformações em funções


