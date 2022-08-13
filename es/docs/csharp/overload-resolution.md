---
title: "Resolución de sobrecarga"
slug: "resolucion-de-sobrecarga"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

El proceso de resolución de sobrecarga se describe en la [especificación de C#][1], sección 7.5.3. También son relevantes las secciones 7.5.2 (inferencia de tipos) y 7.6.5 (expresiones de invocación).

La forma en que funciona la resolución de sobrecarga probablemente cambiará en C# 7. Las notas de diseño indican que Microsoft implementará un nuevo sistema para determinar qué método es mejor (en escenarios complicados).


[1]: https://www.microsoft.com/en-us/download/details.aspx?id=7029

## Ejemplo básico de sobrecarga
Este código contiene un método sobrecargado llamado **Hola**:

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

Cuando se llama al método **Principal**, imprimirá

    int
    double

En tiempo de compilación, cuando el compilador encuentra la llamada al método `Hello(0)`, encuentra todos los métodos con el nombre `Hello`. En este caso, encuentra dos de ellos. Luego intenta determinar cuál de los métodos es *mejor*. El algoritmo para determinar qué método es mejor es complejo, pero generalmente se reduce a "hacer la menor cantidad posible de conversiones implícitas".

Por lo tanto, en el caso de `Hola(0)`, no se necesita conversión para el método `Hola(int)` pero se necesita una conversión numérica implícita para el método `Hola(doble)`. Por lo tanto, el compilador elige el primer método.

En el caso de `Hello(0.0)`, no hay forma de convertir `0.0` a un `int` implícitamente, por lo que el método `Hello(int)` ni siquiera se considera para la resolución de sobrecarga. Solo queda el método y, por lo tanto, el compilador lo elige.

## "params" no se expande, a menos que sea necesario.
El siguiente programa:

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

imprimirá:

    5
    two
    3

La expresión de llamada `Method(objectArray)` podría interpretarse de dos maneras: un solo argumento `Object` que resulta ser una matriz (por lo que el programa generaría `1` porque ese sería el número de argumentos, o como una matriz de argumentos, dados en la forma normal, como si el método `Method` no tuviera la palabra clave `params`. En estas situaciones, la forma normal, no expandida, siempre tiene prioridad. Entonces, el programa genera `5`.

En la segunda expresión, `Method(objectArray, objectArray)`, se aplican tanto la forma expandida del primer método como el segundo método tradicional. En este caso también tienen prioridad las formas no expandidas, por lo que el programa imprime `dos`.

En la tercera expresión, `Method(objectArray, objectArray, objectArray)`, la única opción es usar la forma expandida del primer método, por lo que el programa imprime `3`.

## Pasar nulo como uno de los argumentos
Si usted tiene

    void F1(MyType1 x) {
        // do something
    }

    void F1(MyType2 x) {
        // do something else
    }

y por alguna razón necesita llamar a la primera sobrecarga de `F1` pero con `x = null`, luego simplemente haga

    F1(null);

no compilará ya que la llamada es ambigua. Para contrarrestar esto puedes hacer

    F1(null as MyType1);

