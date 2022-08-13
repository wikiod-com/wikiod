---
title: "Resolução de sobrecarga"
slug: "resolucao-de-sobrecarga"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

O processo de resolução de sobrecarga é descrito na [especificação C#][1], seção 7.5.3. Também são relevantes as seções 7.5.2 (inferência de tipo) e 7.6.5 (expressões de invocação).

O funcionamento da resolução de sobrecarga provavelmente será alterado no C# 7. As notas de design indicam que a Microsoft lançará um novo sistema para determinar qual método é melhor (em cenários complicados).


[1]: https://www.microsoft.com/en-us/download/details.aspx?id=7029

## Exemplo de sobrecarga básica
Este código contém um método sobrecarregado chamado **Hello**:

    class Example
    {
        public static void Hello(int arg)
        {
            Console.WriteLine("int");
        }
     
        public static void Hello(double arg)
        {
            Console.WriteLine("double");
        }
     
        public static void Main(string[] args) 
        {
            Hello(0);
            Hello(0.0);
        }
    }

Quando o método **Main** for chamado, ele imprimirá

    int
    double

Em tempo de compilação, quando o compilador encontra a chamada de método `Hello(0)`, ele encontra todos os métodos com o nome `Hello`. Neste caso, ele encontra dois deles. Em seguida, ele tenta determinar qual dos métodos é *melhor*. O algoritmo para determinar qual método é melhor é complexo, mas geralmente se resume a "fazer o menor número possível de conversões implícitas".

Assim, no caso de `Hello(0)`, nenhuma conversão é necessária para o método `Hello(int)` mas uma conversão numérica implícita é necessária para o método `Hello(double)`. Assim, o primeiro método é escolhido pelo compilador.

No caso de `Hello(0.0)`, não há como converter `0.0` para um `int` implicitamente, então o método `Hello(int)` nem é considerado para resolução de sobrecarga. Apenas o método permanece e por isso é escolhido pelo compilador.

## "params" não é expandido, a menos que seja necessário.
O seguinte programa:

    class Program
    {
        static void Method(params Object[] objects)
        {
            System.Console.WriteLine(objects.Length);
        }   
        static void Method(Object a, Object b)
        {
            System.Console.WriteLine("two");
        }
        static void Main(string[] args)
        {
            object[] objectArray = new object[5];

            Method(objectArray);
            Method(objectArray, objectArray);
            Method(objectArray, objectArray, objectArray);
        }
    }

irá imprimir:

    5
    two
    3

A expressão de chamada `Method(objectArray)` pode ser interpretada de duas maneiras: um único argumento `Object` que é um array (assim o programa produziria `1` porque esse seria o número de argumentos, ou como um array de argumentos, dados na forma normal, como se o método `Method` não tivesse a palavra-chave `params`. Nessas situações, a forma normal, não expandida, sempre tem precedência. Assim, o programa gera `5`.

Na segunda expressão, `Method(objectArray, objectArray)`, tanto a forma expandida do primeiro método quanto o segundo método tradicional são aplicáveis. Neste caso também, os formulários não expandidos têm precedência, então o programa imprime `two`.

Na terceira expressão, `Method(objectArray, objectArray, objectArray)`, a única opção é usar a forma expandida do primeiro método, e assim o programa imprime `3`.

## Passando null como um dos argumentos
Se você tem

    void F1(MyType1 x) {
        // do something
    }

    void F1(MyType2 x) {
        // do something else
    }

e por algum motivo você precisa chamar a primeira sobrecarga de `F1` mas com `x = null`, então simplesmente

    F1(null);

não irá compilar porque a chamada é ambígua. Para combater isso você pode fazer

    F1(null as MyType1);

