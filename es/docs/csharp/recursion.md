---
title: "recursividad"
slug: "recursividad"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Tenga en cuenta que el uso de recursividad puede tener un impacto severo en su código, ya que cada llamada de función recursiva se agregará a la pila. Si hay demasiadas llamadas, esto podría generar una excepción **StackOverflow**. La mayoría de las "funciones recursivas naturales" se pueden escribir como una construcción de bucle `for`, `while` o `foreach`, y aunque no parezca tan **elegante** o **inteligente** será más eficiente.

Siempre piénselo dos veces y use la recursividad con cuidado; sepa por qué la usa:

- la recursividad debe usarse cuando sabe que la cantidad de llamadas recursivas no es *excesiva*
- *excesivo* significa que depende de la cantidad de memoria disponible
- La recursividad se usa porque es una versión de código más clara y limpia, es más legible que una función iterativa o basada en bucles. A menudo, este es el caso porque proporciona un código más limpio y compacto (también conocido como menos líneas de código).
- pero ten cuidado, ¡puede ser menos eficiente! Por ejemplo, en la recursión de Fibonacci, para calcular el número *nth* en la secuencia, ¡el tiempo de cálculo crecerá exponencialmente!

Si quieres más teoría, por favor lee:
- https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/recursion2.html
- https://en.wikipedia.org/wiki/Recursion#In_computer_science


## Recursión en inglés simple
La recursividad se puede definir como:

> Un método que se llama a sí mismo hasta que se cumple una condición específica.

Un excelente y simple ejemplo de recursividad es un método que obtendrá el factorial de un número dado:

    public int Factorial(int number)
    {
        return number == 0 ? 1 : n * Factorial(number - 1);
    }

En este método, podemos ver que el método tomará un argumento, `número`.

Paso a paso:

Dado el ejemplo, ejecutando `Factorial(4)`

1. ¿Es `número (4) == 1`?
2. No? devuelve `4 * Factorial(número-1)` (3)
3. Debido a que el método se llama una vez más, ahora repite el primer paso usando `Factorial(3)` como el nuevo argumento.
4. Esto continúa hasta que se ejecuta `Factorial(1)` y `number (1) == 1` devuelve 1.
5. En general, el cálculo "construye" `4 * 3 * 2 * 1` y finalmente devuelve 24.

La clave para comprender la recursividad es que el método llama a una *nueva instancia* de sí mismo. Después de regresar, continúa la ejecución de la instancia de llamada.

## Secuencia Fibonacci
Puede calcular un número en la secuencia de Fibonacci usando la recursividad.

Siguiendo la teoría matemática de F(n) = F(n-2) + F(n-1), para cualquier i > 0,

    // Returns the i'th Fibonacci number
    public int fib(int i) {
        if(i <= 2) {
            // Base case of the recursive function.
            // i is either 1 or 2, whose associated Fibonacci sequence numbers are 1 and 1.
            return 1;
        }
        // Recursive case. Return the sum of the two previous Fibonacci numbers.
        // This works because the definition of the Fibonacci sequence specifies
        // that the sum of two adjacent elements equals the next element.
        return  fib(i - 2) + fib(i - 1);
        
    }

    fib(10); // Returns 55

## Describe recursivamente una estructura de objeto


## Uso de la recursividad para obtener el árbol de directorios
Uno de los usos de la recursividad es navegar a través de una estructura de datos jerárquica, como un árbol de directorios del sistema de archivos, sin saber cuántos niveles tiene el árbol o la cantidad de objetos en cada nivel. En este ejemplo, verá cómo usar la recursividad en un árbol de directorios para encontrar todos los subdirectorios de un directorio específico e imprimir todo el árbol en la consola.

    internal class Program
    {
        internal const int RootLevel = 0;
        internal const char Tab = '\t';

        internal static void Main()
        {
            Console.WriteLine("Enter the path of the root directory:");
            var rootDirectorypath = Console.ReadLine();

            Console.WriteLine(
                $"Getting directory tree of '{rootDirectorypath}'");

            PrintDirectoryTree(rootDirectorypath);
            Console.WriteLine("Press 'Enter' to quit...");
            Console.ReadLine();
        }

        internal static void PrintDirectoryTree(string rootDirectoryPath)
        {
            try
            {
                if (!Directory.Exists(rootDirectoryPath))
                {
                    throw new DirectoryNotFoundException(
                        $"Directory '{rootDirectoryPath}' not found.");
                }

                var rootDirectory = new DirectoryInfo(rootDirectoryPath);
                PrintDirectoryTree(rootDirectory, RootLevel);
            }
            catch (DirectoryNotFoundException e)
            {
                Console.WriteLine(e.Message);
            }
        }

        private static void PrintDirectoryTree(
            DirectoryInfo directory, int currentLevel)
        {
            var indentation = string.Empty;
            for (var i = RootLevel; i < currentLevel; i++)
            {
                indentation += Tab;
            }

            Console.WriteLine($"{indentation}-{directory.Name}");
            var nextLevel = currentLevel + 1;
            try
            {
                foreach (var subDirectory in directory.GetDirectories())
                {
                    PrintDirectoryTree(subDirectory, nextLevel);
                }
            }
            catch (UnauthorizedAccessException e)
            {
                Console.WriteLine($"{indentation}-{e.Message}");
            }
        }
    }

Este código es un poco más complicado que el mínimo indispensable para completar esta tarea, ya que incluye verificación de excepciones para manejar cualquier problema con la obtención de los directorios. A continuación encontrará un desglose del código en segmentos más pequeños con explicaciones de cada uno.

`Principal`:

El método principal toma una entrada de un usuario como una cadena, que se utilizará como ruta al directorio raíz. Luego llama al método `PrintDirectoryTree` con esta cadena como parámetro.

`ImprimirÁrbolDirectorio(cadena)`:

Este es el primero de dos métodos que manejan la impresión real del árbol de directorios. Este método toma como parámetro una cadena que representa la ruta al directorio raíz. Comprueba si la ruta es un directorio real y, de no ser así, lanza una `DirectoryNotFoundException` que luego se maneja en el bloque catch. Si la ruta es un directorio real, se crea un objeto `DirectoryInfo` `rootDirectory` a partir de la ruta, y se llama al segundo método `PrintDirectoryTree` con el objeto `rootDirectory` y `RootLevel`, que es una constante entera con un valor de cero

`ImprimirÁrbolDirectorio(InformaciónDirectorio, int)`:

Este segundo método maneja la mayor parte del trabajo. Toma un `DirectoryInfo` y un número entero como parámetros. El `DirectoryInfo` es el directorio actual, y el número entero es la profundidad del directorio relativa a la raíz. Para facilitar la lectura, la salida tiene una sangría para cada nivel profundo en el que se encuentra el directorio actual, de modo que la salida se ve así:

    -Root
        -Child 1
        -Child 2
            -Grandchild 2.1
        -Child 3

Una vez que se imprime el directorio actual, se recuperan sus subdirectorios y luego se llama a este método en cada uno de ellos con un valor de nivel de profundidad de uno más que el actual. Esa parte es la recursión: el método llamándose a sí mismo. El programa se ejecutará de esta manera hasta que haya visitado todos los directorios del árbol. Cuando llegue a un directorio sin subdirectorios, el método regresará automáticamente.

Este método también detecta una excepción `UnauthorizedAccessException`, que se genera si alguno de los subdirectorios del directorio actual está protegido por el sistema. El mensaje de error se imprime en el nivel de sangría actual para mantener la coherencia.

El siguiente método proporciona un enfoque más básico para este problema:

    internal static void PrintDirectoryTree(string directoryName)
    {
        try
        {
            if (!Directory.Exists(directoryName)) return;
            Console.WriteLine(directoryName);
            foreach (var d in Directory.GetDirectories(directoryName))
            {
                PrintDirectoryTree(d);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

Esto no incluye la verificación de errores específica o el formato de salida del primer enfoque, pero efectivamente hace lo mismo. Dado que solo usa cadenas en lugar de `DirectoryInfo`, no puede proporcionar acceso a otras propiedades del directorio, como permisos.

## Cálculo del poder de
El cálculo de la potencia de un número dado también se puede hacer recursivamente.
Dado un número base `n` y un exponente `e`, debemos asegurarnos de dividir el problema en partes al disminuir el exponente `e`.

Ejemplo teórico:

- 2² = 2x2
- 2³ = 2x2x2
o, 2³ = 2² x 2<br/>Ahí está el secreto de nuestro algoritmo recursivo (vea el código a continuación). Se trata de tomar el problema y separarlo en partes más pequeñas y fáciles de resolver.
- **Notas**
- cuando el número base es 0, debemos estar atentos para devolver 0 como 0³ = 0 x 0 x 0
- cuando el exponente es 0, debemos estar atentos para devolver siempre 1, ya que esta es una regla matemática.

Ejemplo de código:

    public int CalcPowerOf(int b, int e) {
        if (b == 0) { return 0; } // when base is 0, it doesn't matter, it will always return 0
        if (e == 0) { return 1; } // math rule, exponent 0 always returns 1
        return b * CalcPowerOf(b, e - 1); // actual recursive logic, where we split the problem, aka: 2³ = 2 * 2² etc..
    }

Pruebas en xUnit para verificar la lógica:<br/>
Aunque esto no es necesario, siempre es bueno escribir pruebas para verificar su lógica. Incluyo aquellos aquí escritos en el [marco xUnit][1].

        [Theory]
        [MemberData(nameof(PowerOfTestData))]
        public void PowerOfTest(int @base, int exponent, int expected) {
            Assert.Equal(expected, CalcPowerOf(@base, exponent));
        }

        public static IEnumerable<object[]> PowerOfTestData() {
            yield return new object[] { 0, 0, 0 };
            yield return new object[] { 0, 1, 0 };
            yield return new object[] { 2, 0, 1 };
            yield return new object[] { 2, 1, 2 };
            yield return new object[] { 2, 2, 4 };
            yield return new object[] { 5, 2, 25 };
            yield return new object[] { 5, 3, 125 };
            yield return new object[] { 5, 4, 625 };
    }


[1]: https://xunit.github.io/

## Cálculo factorial
El factorial de un número (denotado con !, como por ejemplo 9!) es la multiplicación de ese número con el factorial de uno menor. Entonces, por ejemplo, ¡9! = 9 x 8! = 9x8x7! = 9 x 8 x 7 x 6 x 5 x 4 x 3 x 2 x 1.

Entonces, en el código que se convierte, usando recursividad:

    long Factorial(long x)
    {
        if (x < 1)
        {
            throw new OutOfRangeException("Factorial can only be used with positive numbers.");
        }
    
        if (x == 1)
        {
            return 1;
        } else {
            return x * Factorial(x - 1);
        }
    }



