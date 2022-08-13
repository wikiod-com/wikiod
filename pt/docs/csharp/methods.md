---
title: "Métodos"
slug: "metodos"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Chamando um método
Chamando um método estático:

    // Single argument
    System.Console.WriteLine("Hello World");  

    // Multiple arguments
    string name = "User";
    System.Console.WriteLine("Hello, {0}!", name);  

Chamando um método estático e armazenando seu valor de retorno:

    string input = System.Console.ReadLine();

Chamando um método de instância:

    int x = 42;
    // The instance method called here is Int32.ToString()
    string xAsString = x.ToString();

Chamando um método genérico

    // Assuming a method 'T[] CreateArray<T>(int size)'
    DateTime[] dates = CreateArray<DateTime>(8);

## Método anônimo
Os métodos anônimos fornecem uma técnica para passar um bloco de código como um parâmetro delegado. São métodos com corpo, mas sem nome.
    
    
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

## Declarando um método
Cada método tem uma assinatura única que consiste em um acessador (`public`, `private`, ...) ,modificador opcional (`abstract`), um nome e, se necessário, parâmetros de método.
Observe que o tipo de retorno não faz parte da assinatura. Um protótipo de método se parece com o seguinte:

    AccessModifier OptionalModifier ReturnType MethodName(InputParameters)
    {
        //Method body
    }

`AccessModifier` pode ser `public`, `protected`, `pirvate` ou por padrão `internal`.

`OptionalModifier` pode ser `static` `abstract` `virtual` `override` `new` ou `sealed`.

`ReturnType` pode ser `void` sem retorno ou pode ser qualquer tipo desde os básicos, como `int` para classes complexas.

um Método pode ter alguns ou nenhum parâmetro de entrada. para definir parâmetros para um método, você deve declarar cada um como declarações de variáveis ​​normais (como `int a`), e para mais de um parâmetro você deve usar vírgula entre eles (como `int a, int b`).

Os parâmetros podem ter valores padrão. para isso você deve definir um valor para o parâmetro (como `int a = 0`). se um parâmetro tiver um valor padrão, a configuração do valor de entrada é opcional.

O exemplo de método a seguir retorna a soma de dois números inteiros:

    private int Sum(int a, int b)
    {
        return a + b;
    } 


## Parâmetros e argumentos
Um método pode declarar qualquer número de parâmetros (neste exemplo, `i`, `s` e `o` são os parâmetros):

    static void DoSomething(int i, string s, object o) {
        Console.WriteLine(String.Format("i={0}, s={1}, o={2}", i, s, o));
    }

Os parâmetros podem ser usados ​​para passar valores para um método, para que o método possa trabalhar com eles. Isso pode ser todo tipo de trabalho, como imprimir os valores, fazer modificações no objeto referenciado por um parâmetro ou armazenar os valores.

Ao chamar o método, você precisa passar um valor real para cada parâmetro. Neste ponto, os valores que você realmente passa para a chamada do método são chamados de Argumentos:

    DoSomething(x, "hello", new object());



## Tipos de retorno
Um método pode retornar nada (`void`) ou um valor de um tipo especificado:

    // If you don't want to return a value, use void as return type.
    static void ReturnsNothing() { 
        Console.WriteLine("Returns nothing");
    }

    // If you want to return a value, you need to specify its type.
    static string ReturnsHelloWorld() {
        return "Hello World";
    }

Se seu método especifica um valor de retorno, o método *deve* retornar um valor. Você faz isso usando a instrução `return`. Uma vez que uma instrução `return` é alcançada, ela retorna o valor especificado e qualquer código após ela não será mais executado (exceções são blocos `finally`, que ainda serão executados antes do retorno do método).

Se o seu método não retornar nada (`void`), você ainda poderá usar a instrução `return` sem um valor se quiser retornar do método imediatamente. No final de tal método, uma instrução `return` seria desnecessária.

Exemplos de instruções `return` válidas:

    return; 
    return 0; 
    return x * 2;
    return Console.ReadLine();

Lançar uma exceção pode encerrar a execução do método sem retornar um valor. Além disso, existem blocos iteradores, onde os valores de retorno são gerados usando a palavra-chave yield, mas esses são casos especiais que não serão explicados neste ponto.

## Parâmetros padrão
Você pode usar os parâmetros padrão se quiser fornecer a opção de deixar os parâmetros de fora:

    static void SaySomething(string what = "ehh") {
        Console.WriteLine(what);
    }  

    static void Main() {
        // prints "hello"
        SaySomething("hello"); 
        // prints "ehh"
        SaySomething(); // The compiler compiles this as if we had typed SaySomething("ehh")
    }

Quando você chama esse método e omite um parâmetro para o qual um valor padrão é fornecido, o compilador insere esse valor padrão para você.

Tenha em mente que os parâmetros com valores padrão precisam ser escritos **depois** dos parâmetros sem valores padrão.

    static void SaySomething(string say, string what = "ehh") {
            //Correct
            Console.WriteLine(say + what);
        }

    static void SaySomethingElse(string what = "ehh", string say) {
            //Incorrect
            Console.WriteLine(say + what);
        }   

**AVISO**: Como funciona dessa forma, os valores padrão podem ser problemáticos em alguns casos. Se você alterar o valor padrão de um parâmetro de método e não recompilar todos os chamadores desse método, esses chamadores ainda usarão o valor padrão que estava presente quando foram compilados, possivelmente causando inconsistências.

## Sobrecarga de métodos
**Definição:** quando vários métodos com o mesmo nome são declarados com parâmetros diferentes, isso é chamado de sobrecarga de método. A sobrecarga de método normalmente representa funções idênticas em sua finalidade, mas são escritas para aceitar diferentes tipos de dados como seus parâmetros.

**Fatores que afetam**

- Número de argumentos
- Tipo de argumentos
- Tipo de retorno**

Considere um método chamado `Area` que realizará funções de cálculo, que aceitará vários argumentos e retornará o resultado.

**Exemplo**

    public string Area(int value1)
    {
        return String.Format("Area of Square is {0}", value1 * value1);
    }
Este método aceitará um argumento e retornará uma string, se chamarmos o método com um inteiro (digamos `5`) a saída será `"Area of ​​Square is 25"`.

    public  double Area(double value1, double value2)
    {
        return value1 * value2;
    }
Da mesma forma, se passarmos dois valores double para este método, a saída será o produto dos dois valores e são do tipo double. Isso pode ser usado para multiplicação, bem como para encontrar a área de retângulos

    public double Area(double value1)
    {
        return 3.14 * Math.Pow(value1,2);
    }
Isso pode ser usado especialmente para encontrar a área do círculo, que aceitará um valor duplo (`raio`) e retornará outro valor duplo como sua Área.

Cada um desses métodos pode ser chamado normalmente sem conflito - o compilador examinará os parâmetros de cada chamada de método para determinar qual versão de `Area` precisa ser usada.

    string squareArea = Area(2);
    double rectangleArea = Area(32.0, 17.5);
    double circleArea = Area(5.0); // all of these are valid and will compile.


----------


**Observe que o tipo de retorno *sozinho* não pode diferenciar entre dois métodos. Por exemplo, se tivéssemos duas definições para Área que tivessem os mesmos parâmetros, assim:

    public string Area(double width, double height) { ... }
    public double Area(double width, double height) { ... }
    // This will NOT compile. 

Se precisarmos que nossa classe use os mesmos nomes de métodos que retornam valores diferentes, podemos remover os problemas de ambiguidade implementando uma interface e definindo explicitamente seu uso.

    public interface IAreaCalculatorString {
        
        public string Area(double width, double height);

    }

    public class AreaCalculator : IAreaCalculatorString {

        public string IAreaCalculatorString.Area(double width, double height) { ... } 
        // Note that the method call now explicitly says it will be used when called through
        // the IAreaCalculatorString interface, allowing us to resolve the ambiguity.
        public double Area(double width, double height) { ... }


## Direitos de acesso
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

