---
title: "Métodos"
slug: "metodos"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Llamar a un método
Llamando a un método estático:

    // Single argument
    System.Console.WriteLine("Hello World");  

    // Multiple arguments
    string name = "User";
    System.Console.WriteLine("Hello, {0}!", name);  

Llamar a un método estático y almacenar su valor de retorno:

    string input = System.Console.ReadLine();

Llamar a un método de instancia:

    int x = 42;
    // The instance method called here is Int32.ToString()
    string xAsString = x.ToString();

Llamar a un método genérico

    // Assuming a method 'T[] CreateArray<T>(int size)'
    DateTime[] dates = CreateArray<DateTime>(8);

## Método anónimo
Los métodos anónimos proporcionan una técnica para pasar un bloque de código como parámetro de delegado. Son métodos con cuerpo, pero sin nombre.
    
    
    delegate int IntOp(int lhs, int rhs);

<!-- separador -->

    class Program
    {
        static void Main(string[] args)
        {
            // C# 2.0 definition
            IntOp add = delegate(int lhs, int rhs)
            {
                return lhs + rhs;
            };

            // C# 3.0 definition
            IntOp mul = (lhs, rhs) =>
            {
                return lhs * rhs;
            };

            // C# 3.0 definition - shorthand
            IntOp sub = (lhs, rhs) => lhs - rhs;

            // Calling each method
            Console.WriteLine("2 + 3 = " + add(2, 3));
            Console.WriteLine("2 * 3 = " + mul(2, 3));
            Console.WriteLine("2 - 3 = " + sub(2, 3));
        }
    }

## Declarando un método
Cada método tiene una firma única que consta de un elemento de acceso (`público`, `privado`, ...), un modificador opcional (`abstracto`), un nombre y, si es necesario, parámetros del método.
Tenga en cuenta que el tipo de devolución no forma parte de la firma. Un prototipo de método tiene el siguiente aspecto:

    AccessModifier OptionalModifier ReturnType MethodName(InputParameters)
    {
        //Method body
    }

`AccessModifier` puede ser `público`, `protegido`, `privado` o por defecto `interno`.

`OptionalModifier` puede ser `static` `abstract` `virtual` `override` `new` o `sealed`.

`ReturnType` puede ser `void` para no retorno o puede ser cualquier tipo desde los básicos, como `int` a clases complejas.

un método puede tener algunos o ningún parámetro de entrada. para establecer parámetros para un método, debe declarar cada uno como declaraciones de variables normales (como `int a`), y para más de un parámetro debe usar una coma entre ellos (como `int a, int b`).

Los parámetros pueden tener valores predeterminados. para esto, debe establecer un valor para el parámetro (como `int a = 0`). si un parámetro tiene un valor predeterminado, establecer el valor de entrada es opcional.

El siguiente ejemplo de método devuelve la suma de dos enteros:

    private int Sum(int a, int b)
    {
        return a + b;
    } 


## Parámetros y Argumentos
Un método puede declarar cualquier cantidad de parámetros (en este ejemplo, `i`, `s` y `o` son los parámetros):

    static void DoSomething(int i, string s, object o) {
        Console.WriteLine(String.Format("i={0}, s={1}, o={2}", i, s, o));
    }

Los parámetros se pueden usar para pasar valores a un método, de modo que el método pueda trabajar con ellos. Esto puede ser todo tipo de trabajo, como imprimir los valores, hacer modificaciones al objeto al que hace referencia un parámetro o almacenar los valores.

Cuando llama al método, debe pasar un valor real para cada parámetro. En este punto, los valores que realmente pasa a la llamada al método se denominan Argumentos:

    DoSomething(x, "hello", new object());



## Tipos de devolución
Un método puede devolver nada ("void") o un valor de un tipo específico:

    // If you don't want to return a value, use void as return type.
    static void ReturnsNothing() { 
        Console.WriteLine("Returns nothing");
    }

    // If you want to return a value, you need to specify its type.
    static string ReturnsHelloWorld() {
        return "Hello World";
    }

Si su método especifica un valor devuelto, el método *debe* devolver un valor. Haces esto usando la instrucción `return`. Una vez que se ha llegado a una instrucción `return`, devuelve el valor especificado y cualquier código posterior ya no se ejecutará (las excepciones son los bloques `finally`, que aún se ejecutarán antes de que el método regrese).

Si su método no devuelve nada (`void`), aún puede usar la instrucción `return` sin un valor si desea regresar del método inmediatamente. Sin embargo, al final de dicho método, una declaración `return` sería innecesaria.

Ejemplos de sentencias `return` válidas:

    return; 
    return 0; 
    return x * 2;
    return Console.ReadLine();

Lanzar una excepción puede finalizar la ejecución del método sin devolver un valor. Además, hay bloques iteradores, donde los valores devueltos se generan usando la palabra clave yield, pero esos son casos especiales que no se explicarán en este punto.

## Parámetros predeterminados
Puede usar parámetros predeterminados si desea proporcionar la opción de omitir parámetros:

    static void SaySomething(string what = "ehh") {
        Console.WriteLine(what);
    }  

    static void Main() {
        // prints "hello"
        SaySomething("hello"); 
        // prints "ehh"
        SaySomething(); // The compiler compiles this as if we had typed SaySomething("ehh")
    }

Cuando llama a un método de este tipo y omite un parámetro para el que se proporciona un valor predeterminado, el compilador inserta ese valor predeterminado por usted.

Tenga en cuenta que los parámetros con valores predeterminados deben escribirse **después** de los parámetros sin valores predeterminados.

    static void SaySomething(string say, string what = "ehh") {
            //Correct
            Console.WriteLine(say + what);
        }

    static void SaySomethingElse(string what = "ehh", string say) {
            //Incorrect
            Console.WriteLine(say + what);
        }   

**ADVERTENCIA**: Debido a que funciona de esa manera, los valores predeterminados pueden ser problemáticos en algunos casos. Si cambia el valor predeterminado de un parámetro de método y no vuelve a compilar todas las llamadas de ese método, esas llamadas aún usarán el valor predeterminado que estaba presente cuando se compilaron, lo que posiblemente cause inconsistencias.

## Sobrecarga de métodos
**Definición:** Cuando se declaran varios métodos con el mismo nombre con diferentes parámetros, se denomina sobrecarga de métodos. La sobrecarga de métodos generalmente representa funciones que son idénticas en su propósito pero están escritas para aceptar diferentes tipos de datos como sus parámetros.

**Los factores que afectan**

- Número de argumentos
- Tipo de argumentos
- Tipo de devolución**

Considere un método llamado `Área` que realizará funciones de cálculo, que aceptará varios argumentos y devolverá el resultado.

**Ejemplo**

    public string Area(int value1)
    {
        return String.Format("Area of Square is {0}", value1 * value1);
    }
Este método aceptará un argumento y devolverá una cadena, si llamamos al método con un número entero (digamos `5`), la salida será `"El área del cuadrado es 25"`.

    public  double Area(double value1, double value2)
    {
        return value1 * value2;
    }
De manera similar, si pasamos dos valores dobles a este método, la salida será el producto de los dos valores y serán de tipo doble. Esto se puede usar tanto para la multiplicación como para encontrar el área de los rectángulos.

    public double Area(double value1)
    {
        return 3.14 * Math.Pow(value1,2);
    }
Esto se puede usar especialmente para encontrar el área del círculo, que aceptará un valor doble (`radio`) y devolverá otro valor doble como su Área.

Cada uno de estos métodos se puede llamar normalmente sin conflicto: el compilador examinará los parámetros de cada llamada de método para determinar qué versión de `Area` se necesita usar.

    string squareArea = Area(2);
    double rectangleArea = Area(32.0, 17.5);
    double circleArea = Area(5.0); // all of these are valid and will compile.


----------


**Tenga en cuenta que el tipo de retorno *solo* no puede diferenciar entre dos métodos. Por ejemplo, si tuviéramos dos definiciones de Área que tuvieran los mismos parámetros, así:

    public string Area(double width, double height) { ... }
    public double Area(double width, double height) { ... }
    // This will NOT compile. 

Si necesitamos que nuestra clase use los mismos nombres de métodos que devuelven valores diferentes, podemos eliminar los problemas de ambigüedad implementando una interfaz y definiendo explícitamente su uso.

    public interface IAreaCalculatorString {
        
        public string Area(double width, double height);

    }

    public class AreaCalculator : IAreaCalculatorString {

        public string IAreaCalculatorString.Area(double width, double height) { ... } 
        // Note that the method call now explicitly says it will be used when called through
        // the IAreaCalculatorString interface, allowing us to resolve the ambiguity.
        public double Area(double width, double height) { ... }


## Derechos de acceso
    // static: is callable on a class even when no instance of the class has been created
    public static void MyMethod()

    // virtual: can be called or overridden in an inherited class
    public virtual  void MyMethod()

    // internal: access is limited within the current assembly
    internal  void MyMethod()

    //private: access is limited only within the same class
    private  void MyMethod()

    //public: access right from every class / assembly
    public void MyMethod()

    //protected: access is limited to the containing class or types derived from it
    protected void MyMethod()

    //protected internal: access is limited to the current assembly or types derived from the containing class.
    protected internal void MyMethod()

