---
title: "Operadores"
slug: "operadores"
draft: false
images: []
weight: 9543
type: docs
toc: true
---

Em C#, um [operador](https://docs.microsoft.com/en-us/dotnet/csharp/program) é um elemento de programa aplicado a um ou mais operandos em uma expressão ou instrução. Os operadores que usam um operando, como o operador de incremento (++) ou novo, são chamados de operadores unários. Operadores que aceitam dois operandos, como operadores aritméticos (+,-,*,/), são chamados de operadores binários. Um operador, o operador condicional (?:), recebe três operandos e é o único operador ternário em C#.



## Sintaxe
- operador OperandType estático público operatorSymbol(OperandType operando1)
- operador OperandType estático público operatorSymbol(OperandType operando1, OperandType2 operando2)

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| operadorSímbolo | O operador sendo sobrecarregado, e. +, -, /, * |
| Tipo de Operando | O tipo que será retornado pelo operador sobrecarregado.
| operando1 | O primeiro operando a ser usado na execução da operação.
| operando2 | O segundo operando a ser utilizado na execução da operação, ao realizar operações binárias.
| declarações | Código opcional necessário para realizar a operação antes de retornar o resultado.

Todos os operadores são definidos como `métodos estáticos` e não são `virtuais` e não são herdados.
### Operador precedente

Todos os operadores têm uma "precedência" particular dependendo de qual grupo o operador se enquadra (operadores do mesmo grupo têm igual precedência). Ou seja, alguns operadores serão aplicados antes de outros. O que segue é uma lista de grupos (contendo seus respectivos operadores) ordenados por precedência (o mais alto primeiro):

* **Operadores Primários**
* `a.b` - Acesso de membro.
* `a?.b` - Acesso de membro condicional nulo.
* `->` - Desreferenciamento de ponteiro combinado com acesso de membro.
* `f(x)` - Invocação de função.
* `a[x]` - Indexador.
* `a?[x]` - Indexador condicional nulo.
* `x++` - Incremento pós-fixo.
* `x--` - Decremento pós-fixado.
* `new` - Instanciação do tipo.
* `default(T)` - Retorna o valor inicializado padrão do tipo `T`.
* `typeof` - Retorna o objeto `Type` do operando.
* `checked` - Habilita a verificação de estouro numérico.
* `unchecked` - Desativa a verificação de estouro numérico.
* `delegate` - Declara e retorna uma instância delegada.
* `sizeof` - Retorna o tamanho em bytes do operando tipo.

* **Operadores Unários**
* `+x` - Retorna `x`.
* `-x` - Negação numérica.
* `!x` - Negação lógica.
* `~x` - Complementa/declara destruidores bit a bit.
* `++x` - Incremento de prefixo.
* `--x` - Decremento de prefixo.
* `(T)x` - conversão de tipo.
* `await` - Aguarda uma `Task`.
* `&x` - Retorna o endereço (ponteiro) de `x`.
* `*x` - Desreferenciamento do ponteiro.

* **Operadores Multiplicativos**
* `x * y` - Multiplicação.
* `x / y` - Divisão.
* `x % y` - Módulo.

* **Operadores Aditivos**
* `x + y` - Adição.
* `x – y` - Subtração.

* **Operadores de deslocamento bit a bit**
* `x << y` - Desloca bits para a esquerda.
* `x >> y` - Desloca bits para a direita.

* **Operadores relacionais/de teste de tipo**
* `x < y` - Menor que.
* `x > y` - Maior que.
* `x <= y` - Menor ou igual a.
* `x >= y` - Maior ou igual a.
* `is` - Compatibilidade de tipo.
* `as` - Conversão de tipo.

* **Operadores de igualdade**
* `x == y` - Igualdade.
* `x != y` - Diferente.

* **Operador E Lógico**
* `x & y` - E lógico/bit a bit.

* **Operador XOR Lógico**
* `x ^ y` - XOR lógico/bit a bit.

* **Operador OR lógico**
* `x | y` - OU lógico/bit a bit.

* ** Operador E Condicional **
* `x && y` - E lógico em curto-circuito.

* **Operador OU Condicional**
* `x || y` - OR lógico de curto-circuito.

* **Operador de coalescência nula**
* `x ?? y` - Retorna `x` se não for nulo; caso contrário, retorna `y`.

* **Operador Condicional**
* `x ? y : z` - Avalia/retorna `y` se `x` for verdadeiro; caso contrário, avalia `z`.


---

**Conteúdo Relacionado**

- [Operador de coalescência nula][1]
- [Operador Condicional Nulo][2]
- [nome do Operador][3]

[1]: https://www.wikiod.com/pt/docs/c%23/37/null-coalescing-operator#t=201511232329424573937
[2]: https://www.wikiod.com/pt/docs/c%23/41/the-null-conditional-operator#t=201511232329445644147
[3]: https://www.wikiod.com/pt/docs/c%23/80/nameof-operator#t=201608081725023270827

## Operadores Sobrecarregáveis
C# permite que tipos definidos pelo usuário sobrecarreguem os operadores definindo funções de membro estáticas usando a palavra-chave `operator`.
O exemplo a seguir ilustra uma implementação do operador `+`.

Se tivermos uma classe `Complex` que representa um número complexo:

    public struct Complex
    {
        public double Real { get; set; }
        public double Imaginary { get; set; }
    }

E queremos adicionar a opção de usar o operador `+` para esta classe. ou seja:

    Complex a = new Complex() { Real = 1, Imaginary = 2 };
    Complex b = new Complex() { Real = 4, Imaginary = 8 };
    Complex c = a + b;

Precisaremos sobrecarregar o operador `+` para a classe. Isso é feito usando uma função estática e a palavra-chave `operator`:

    public static Complex operator +(Complex c1, Complex c2)
    {
       return new Complex 
       { 
           Real = c1.Real + c2.Real,
           Imaginary = c1.Imaginary + c2.Imaginary 
       };
    }

Operadores como `+`, `-`, `*`, `/` podem ser sobrecarregados. Isso também inclui Operadores que não retornam o mesmo tipo (por exemplo, `==` e `!=` podem ser sobrecarregados, apesar de retornar booleanos) A regra abaixo relacionada a pares também é aplicada aqui.

Os operadores de comparação precisam ser sobrecarregados em pares (por exemplo, se `<` estiver sobrecarregado, `>` também precisa ser sobrecarregado).

Uma lista completa de operadores sobrecarregáveis ​​(assim como operadores não sobrecarregáveis ​​e as restrições impostas a alguns operadores sobrecarregáveis) pode ser vista em [MSDN - Overloadable Operators (C# Programming Guide)][1].

<!-- if versão [gte 7.0] -->
a sobrecarga de `operator is` foi introduzida com o mecanismo de correspondência de padrões do C# 7.0. Para obter detalhes, consulte [Correspondência de padrões][2]

Dado um tipo `Cartesiano` definido da seguinte forma

    public class Cartesian
    {
        public int X { get; }
        public int Y { get; }
    }   

Um `operador é` sobrecarregado poderia, por exemplo, ser definido para coordenadas `Polar`

    public static class Polar
    {
        public static bool operator is(Cartesian c, out double R, out double Theta)
        {
            R = Math.Sqrt(c.X*c.X + c.Y*c.Y);
            Theta = Math.Atan2(c.Y, c.X);
            return c.X != 0 || c.Y != 0;
        }
    }

que pode ser usado assim

    var c = Cartesian(3, 4);
    if (c is Polar(var R, *))
    {
        Console.WriteLine(R);
    }

(O exemplo foi retirado da [Documentação de correspondência de padrões Roslyn][3])
<!-- versão final if -->

[1]: https://msdn.microsoft.com/en-us/library/8edha89s.aspx
[2]: https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/13323/pattern-matching#t=201608081959042378203
[3]: https://github.com/dotnet/roslyn/blob/future/docs/features/patterns.md

## Sobrecarregando operadores de igualdade
Sobrecarregar apenas operadores de igualdade não é suficiente. Sob diferentes circunstâncias, todos os itens a seguir podem ser chamados:

1. `object.Equals` e `object.GetHashCode`
2. `IEquatable<T>.Equals` (opcional, permite evitar boxe)
3. `operator ==` e `operator !=` (opcional, permite o uso de operadores)

Ao substituir `Equals`, `GetHashCode` também deve ser substituído. Ao implementar `Equals`, existem muitos casos especiais: comparação com objetos de um tipo diferente, comparação com self etc.

Quando NÃO é substituído o método `Equals` e o operador `==` se comportam de forma diferente para classes e structs. Para classes apenas referências são comparadas, e para structs valores de propriedades são comparados via reflexão o que pode afetar negativamente o desempenho. `==` não pode ser usado para comparar structs a menos que seja substituído.

Geralmente a operação de igualdade deve obedecer às seguintes regras:

- Não deve *lançar exceções*.
- Reflexividade: `A` sempre é igual a `A` (pode não ser verdade para valores `NULL` em alguns sistemas).
- Transitividade: se `A` é igual a `B`, e `B` é igual a `C`, então `A` é igual a `C`.
- Se `A` for igual a `B`, então `A` e `B` terão códigos hash iguais.
- Independência da árvore de herança: se `B` e `C` são instâncias de `Class2` herdadas de `Class1`:
`Class1.Equals(A,B)` deve sempre retornar o mesmo valor que a chamada para `Class2.Equals(A,B)`.
      

    class Student : IEquatable<Student>
    {
        public string Name { get; set; } = "";
    
        public bool Equals(Student other)
        {
            if (ReferenceEquals(other, null)) return false;
            if (ReferenceEquals(other, this)) return true;
            return string.Equals(Name, other.Name);
        }
    
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;

            return Equals(obj as Student);
        }
    
        public override int GetHashCode()
        {
            return Name?.GetHashCode() ?? 0;
        }
    
        public static bool operator ==(Student left, Student right)
        {
            return Equals(left, right);
        }
    
        public static bool operator !=(Student left, Student right)
        {
            return !Equals(left, right);
        }
    }

## Operadores Relacionais
**É igual a**

Verifica se os operandos fornecidos (argumentos) são iguais

    "a" == "b"     // Returns false.
    "a" == "a"     // Returns true.
    1 == 0         // Returns false.
    1 == 1         // Returns true.
    false == true  // Returns false.
    false == false // Returns true.

Ao contrário do Java, o operador de comparação de igualdade funciona nativamente com strings.

O operador de comparação de igualdade funcionará com operandos de tipos diferentes se existir uma conversão implícita de um para o outro. Se não existir uma conversão implícita adequada, você poderá chamar uma conversão explícita ou usar um método para converter em um tipo compatível.

    1 == 1.0              // Returns true because there is an implicit cast from int to double.
    new Object() == 1.0   // Will not compile.
    MyStruct.AsInt() == 1 // Calls AsInt() on MyStruct and compares the resulting int with 1.

Ao contrário do Visual Basic.NET, o operador de comparação de igualdade não é o mesmo que o operador de atribuição de igualdade.

    var x = new Object();
    var y = new Object();
    x == y // Returns false, the operands (objects in this case) have different references.
    x == x // Returns true, both operands have the same reference.

<sup>*Não confundir com o operador de atribuição (`=`).*</sup>

Para tipos de valor, o operador retorna `true` se ambos os operandos forem iguais em valor.
Para tipos de referência, o operador retorna `true` se ambos os operandos forem iguais em *reference* (não valor). Uma exceção é que objetos string serão comparados com igualdade de valor.

**Não é igual**

Verifica se os operandos fornecidos *não* são iguais.

    "a" != "b"     // Returns true.
    "a" != "a"     // Returns false.
    1 != 0         // Returns true.
    1 != 1         // Returns false.
    false != true  // Returns true.
    false != false // Returns false.

    var x = new Object();
    var y = new Object();
    x != y // Returns true, the operands have different references.
    x != x // Returns false, both operands have the same reference.

Este operador efetivamente retorna o resultado oposto ao do operador equals (`==`)

**Maior que**

Verifica se o primeiro operando é maior que o segundo operando.

    3 > 5    //Returns false.
    1 > 0    //Returns true.
    2 > 2    //Return false.
    
    var x = 10;
    var y = 15;
    x > y    //Returns false.
    y > x    //Returns true.

**Menor que**

Verifica se o primeiro operando é menor que o segundo operando.

    2 < 4     //Returns true.
    1 < -3    //Returns false.
    2 < 2     //Return false.
    
    var x = 12;
    var y = 22;
    x < y    //Returns true.
    y < x    //Returns false.

**Maior que igual a**

Verifica se o primeiro operando é maior que igual ao segundo operando.

    7 >= 8    //Returns false.
    0 >= 0    //Returns true.
    
**Menos que igual a**

Verifica se o primeiro operando é menor que igual ao segundo operando.

    2 <= 4    //Returns true.
    1 <= -3    //Returns false.
    1 <= 1     //Returns true. 
    
  
  

## Operadores de elenco implícito e de elenco explícito
C# permite que tipos definidos pelo usuário controlem a atribuição e a conversão através do uso das palavras-chave `explicit` e `implicit`. A assinatura do método assume a forma:

    public static <implicit/explicit> operator <ResultingType>(<SourceType> myType)

O método não pode receber mais argumentos, nem pode ser um método de instância. Ele pode, no entanto, acessar quaisquer membros privados do tipo em que está definido.

Um exemplo de uma conversão `implícita` e `explícita`:

    public class BinaryImage 
    {
        private bool[] _pixels;

        public static implicit operator ColorImage(BinaryImage im)
        {
            return new ColorImage(im);
        }

        public static explicit operator bool[](BinaryImage im)
        {
            return im._pixels;
        }
    }

Permitindo a seguinte sintaxe de conversão:

    var binaryImage = new BinaryImage();
    ColorImage colorImage = binaryImage; // implicit cast, note the lack of type 
    bool[] pixels = (bool[])binaryImage; // explicit cast, defining the type

Os operadores de conversão podem funcionar nos dois sentidos, indo *de* seu tipo e indo *para* seu tipo:

    public class BinaryImage
    {
        public static explicit operator ColorImage(BinaryImage im)
        {
            return new ColorImage(im);
        }

        public static explicit operator BinaryImage(ColorImage cm)
        {
            return new BinaryImage(cm);
        }
    }

Finalmente, a palavra-chave `as`, que pode estar envolvida na conversão dentro de uma hierarquia de tipos, **não** é válida nesta situação. Mesmo depois de definir um cast `explícito` ou `implícito`, você não pode fazer:

    ColorImage cm = myBinaryImage as ColorImage;

Ele irá gerar um erro de compilação.

## Operadores de curto-circuito
*Por definição, os operadores booleanos de curto-circuito só avaliarão o segundo operando se o primeiro operando não puder determinar o resultado geral da expressão.*

Isso significa que, se você estiver usando o operador && como *firstCondition && secondCondition* ele avaliará *secondCondition* somente quando *firstCondition* for verdadeiro e, claro, o resultado geral será verdadeiro somente se *firstOperand* e *secondOperand* forem avaliados para verdade. Isso é útil em muitos cenários, por exemplo, imagine que você deseja verificar se sua lista tem mais de três elementos, mas também precisa verificar se a lista foi inicializada para não executar *NullReferenceException*. Você pode conseguir isso como abaixo:

    bool hasMoreThanThreeElements = myList != null && mList.Count > 3;

*mList.Count > 3* não será verificado até que myList != null seja atendido.

**Lógico E**

`&&` é a contrapartida de curto-circuito do operador booleano AND (`&`).

    var x = true;
    var y = false;

    x && x // Returns true.
    x && y // Returns false (y is evaluated).
    y && x // Returns false (x is not evaluated).
    y && y // Returns false (right y is not evaluated).

**OR Lógico**

`||` é a contrapartida de curto-circuito do operador booleano padrão OR (`|`).

    var x = true;
    var y = false;

    x || x // Returns true (right x is not evaluated).
    x || y // Returns true (y is not evaluated).
    y || x // Returns true (x and y are evaluated).
    y || y // Returns false (y and y are evaluated).

**Exemplo de uso**

    if(object != null && object.Property)
    // object.Property is never accessed if object is null, because of the short circuit.
        Action1();
    else
        Action2();

##? : Operador Ternário
Retorna um de dois valores dependendo do valor de uma expressão booleana.

Sintaxe:

    condition ? expression_if_true : expression_if_false;

Exemplo:

    string name = "Frank";
    Console.WriteLine(name == "Frank" ? "The name is Frank" : "The name is not Frank");

O operador ternário é associativo à direita, o que permite o uso de expressões ternárias compostas. Isso é feito adicionando equações ternárias adicionais na posição verdadeira ou falsa de uma equação ternária pai. Deve-se tomar cuidado para garantir a legibilidade, mas isso pode ser útil em algumas circunstâncias.

Neste exemplo, uma operação ternária composta avalia uma função `clamp` e retorna o valor atual se estiver dentro do intervalo, o valor `min` se estiver abaixo do intervalo ou o valor `max` se estiver acima do intervalo.

    light.intensity = Clamp(light.intensity, minLight, maxLight);

    public static float Clamp(float val, float min, float max)
    {
        return (val < min) ? min : (val > max) ? max : val;
    }

Os operadores ternários também podem ser aninhados, como:

    a ? b ? "a is true, b is true" : "a is true, b is false" : "a is false"
    
    // This is evaluated from left to right and can be more easily seen with parenthesis:
    
    a ? (b ? x : y) : z

    // Where the result is x if a && b, y if a && !b, and z if !a

Ao escrever declarações ternárias compostas, é comum usar parênteses ou recuo para melhorar a legibilidade.

Os tipos de *expression_if_true* e *expression_if_false* devem ser idênticos ou deve haver uma conversão implícita de um para o outro.

    condition ? 3 : "Not three"; // Doesn't compile because `int` and `string` lack an implicit conversion.

    condition ? 3.ToString() : "Not three"; // OK because both possible outputs are strings.

    condition ? 3 : 3.5; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

    condition ? 3.5 : 3; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

Os requisitos de tipo e conversão também se aplicam às suas próprias classes.

    public class Car
    {}

    public class SportsCar : Car
    {}

    public class SUV : Car
    {}

    condition ? new SportsCar() : new Car(); // OK because there is an implicit conversion from `SportsCar` to `Car`. The ternary operator will return a reference of type `Car`.

    condition ? new Car() : new SportsCar(); // OK because there is an implicit conversion from `SportsCar` to `Car`. The ternary operator will return a reference of type `Car`.

    condition ? new SportsCar() : new SUV(); // Doesn't compile because there is no implicit conversion from `SportsCar` to SUV or `SUV` to `SportsCar`. The compiler is not smart enough to realize that both of them have an implicit conversion to `Car`.

    condition ? new SportsCar() as Car : new SUV() as Car; // OK because both expressions evaluate to a reference of type `Car`. The ternary operator will return a reference of type `Car`.


##?. (Operador Condicional Nulo)
<!-- if versão [gte 6.0] -->

[Introduzido no C# 6.0][1], o operador condicional nulo `?.` retornará imediatamente `null` se a expressão em seu lado esquerdo for avaliada como `null`, em vez de lançar uma `NullReferenceException`. Se seu lado esquerdo for avaliado como um valor não `null`, ele será tratado como um operador `.` normal. Observe que, como ele pode retornar `null`, seu tipo de retorno é sempre um tipo anulável. Isso significa que para um tipo struct ou primitivo, ele é encapsulado em um `Nullable<T>`.

    var bar = Foo.GetBar()?.Value; // will return null if GetBar() returns null
    var baz = Foo.GetBar()?.IntegerValue; // baz will be of type Nullable<int>, i.e. int?
  
Isso é útil ao disparar eventos. Normalmente, você teria que envolver a chamada do evento em uma instrução if verificando se há `null` e gerar o evento depois, o que introduz a possibilidade de uma condição de corrida. Usando o operador condicional Null, isso pode ser corrigido da seguinte maneira:

    event EventHandler<string> RaiseMe;
    RaiseMe?.Invoke("Event raised");

<!-- versão final if -->


[1]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607301051500162149

## tamanho de
Retorna um `int` contendo o tamanho de um tipo<sup>*</sup> em bytes.

    sizeof(bool)    // Returns 1.
    sizeof(byte)    // Returns 1.
    sizeof(sbyte)   // Returns 1.
    sizeof(char)    // Returns 2.
    sizeof(short)   // Returns 2.
    sizeof(ushort)  // Returns 2.
    sizeof(int)     // Returns 4.
    sizeof(uint)    // Returns 4.
    sizeof(float)   // Returns 4.
    sizeof(long)    // Returns 8.
    sizeof(ulong)   // Returns 8.
    sizeof(double)  // Returns 8.
    sizeof(decimal) // Returns 16.

<sup>**Suporta apenas certos tipos primitivos em contexto seguro.*</sup>

Em um contexto inseguro, `sizeof` pode ser usado para retornar o tamanho de outros tipos e estruturas primitivos.

    public struct CustomType
    {
        public int value;
    }

    static void Main()
    {
        unsafe
        {
            Console.WriteLine(sizeof(CustomType)); // outputs: 4
        }
    }

## Operadores de membro de classe: acesso condicional nulo de membro
    var zipcode = myEmployee?.Address?.ZipCode;
    //returns null if the left operand is null.  
    //the above is the equivalent of:
    var zipcode = (string)null;
    if (myEmployee != null && myEmployee.Address != null)
        zipcode = myEmployee.Address.ZipCode;

## Operadores de membros de classe: indexação condicional nula
    var letters = null;
    char? letter = letters?[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example  rather than throwing an error because letters is null
    //letter is assigned the value null

## Operador "Exclusivo ou"
O operador para um "ou exclusivo" (para abreviar XOR) é: ^

Este operador retorna true quando um, mas apenas um, dos bools fornecidos é true.

    true ^ false   // Returns true
    false ^ true   // Returns true
    false ^ false  // Returns false
    true ^ true    // Returns false

## Operadores de deslocamento de bits
Os operadores de deslocamento permitem que os programadores ajustem um inteiro deslocando todos os seus bits para a esquerda ou para a direita. O diagrama a seguir mostra o efeito de deslocar um valor para a esquerda em um dígito.

**Desvio à esquerda**

    uint value = 15;              // 00001111
     
    uint doubled = value << 1;    // Result = 00011110 = 30
    uint shiftFour = value << 4;  // Result = 11110000 = 240

**Deslocamento para a direita**

    uint value = 240;             // 11110000
     
    uint halved = value >> 1;     // Result = 01111000 = 120
    uint shiftFour = value >> 4;  // Result = 00001111 = 15

## Operador padrão
Tipo de valor (onde T : struct)
---
Os tipos de dados primitivos integrados, como `char`, `int` e `float`, bem como os tipos definidos pelo usuário declarados com `struct` ou `enum`. Seu valor padrão é `new T()`:

    default(int)            // 0
    default(DateTime)       // 0001-01-01 12:00:00 AM
    default(char)           // '\0' This is the "null character", not a zero or a line break.
    default(Guid)           // 00000000-0000-0000-0000-000000000000
    default(MyStruct)       // new MyStruct()

    // Note: default of an enum is 0, and not the first *key* in that enum
    // so it could potentially fail the Enum.IsDefined test
    default(MyEnum)         // (MyEnum)0

Tipo de referência (onde T : classe)
---

Qualquer tipo `class`, `interface`, array ou delegado. Seu valor padrão é `null` :

    default(object)         // null
    default(string)         // null
    default(MyClass)        // null
    default(IDisposable)    // null
    default(dynamic)        // null

## Incremento e decremento de Postfix e Prefix
O incremento postfix `X++` adicionará `1` a `x`

    var x = 42;
    x++;
    Console.WriteLine(x); // 43

O decremento do postfix `X--` subtrairá um

    var x = 42
    x--; 
    Console.WriteLine(x); // 41



`++x` é chamado de incremento de prefixo ele incrementa o valor de x e então retorna x
enquanto `x++` retorna o valor de x e então incrementa

    var x = 42;
    Console.WriteLine(++x); // 43
    System.out.println(x); // 43

enquanto

    var x = 42;
    Console.WriteLine(x++); // 42
    System.out.println(x); // 43

ambos são comumente usados ​​em loop for

    for(int i = 0; i < 10; i++)
    {
    }


## => Operador lambda
<!-- if versão [gte 3.0] -->

*O operador `=>` tem a mesma precedência que o operador de atribuição `=` e é associativo à direita.*

Ele é usado para declarar expressões lambda e também é amplamente usado com [Consultas LINQ](https://www.wikiod.com/pt/docs/c%23/68/linq-queries/4735/basics#t=201607251514251028068):

    string[] words = { "cherry", "apple", "blueberry" };

    int shortestWordLength = words.Min((string w) => w.Length); //5

Quando usado em extensões ou consultas LINQ, o tipo dos objetos geralmente pode ser ignorado, pois é inferido pelo compilador:

    int shortestWordLength = words.Min(w => w.Length); //also compiles with the same result

A forma geral do operador lambda é a seguinte:

    (input parameters) => expression

Os parâmetros da expressão lambda são especificados antes do operador `=>`, e a expressão/instrução/bloco real a ser executado está à direita do operador:

    // expression
    (int x, string s) => s.Length > x

    // expression
    (int x, int y) => x + y

    // statement
    (string x) => Console.WriteLine(x)

    // block
    (string x) => {
            x += " says Hello!";
            Console.WriteLine(x);
        }

Este operador pode ser usado para definir delegados facilmente, sem escrever um método explícito:

    delegate void TestDelegate(string s);
    
    TestDelegate myDelegate = s => Console.WriteLine(s + " World");

    myDelegate("Hello");

ao invés de

    void MyMethod(string s)
    {
        Console.WriteLine(s + " World");
    }
    
    delegate void TestDelegate(string s);

    TestDelegate myDelegate = MyMethod;

    myDelegate("Hello");

<!-- versão final if -->

## Operador de atribuição '='
O operador de atribuição `=` define o valor do operando esquerdo para o valor do operando direito e retorna esse valor:

    int a = 3;     // assigns value 3 to variable a
    int b = a = 5; // first assigns value 5 to variable a, then does the same for variable b
    Console.WriteLine(a = 3 + 4); // prints 7


##?? Operador de coalescência nula
O operador Null-Coalescing `??` retornará o lado esquerdo quando não for nulo. Se for nulo, retornará o lado direito.

    object foo = null;
    object bar = new object();
    
    var c = foo ?? bar;
    //c will be bar since foo was null

O operador `??` pode ser encadeado o que permite a remoção de verificações `if`.

    //config will be the first non-null returned.
    var config = RetrieveConfigOnMachine() ??
                 RetrieveConfigFromService() ??
                 new DefaultConfiguration();





## Operadores de membros da classe: Acesso de membros
    var now = DateTime.UtcNow;
    //accesses member of a class.  In this case the UtcNow property.

## Operadores de membro de classe: invocação de função
    var age = GetAge(dateOfBirth);
    //the above calls the function GetAge passing parameter dateOfBirth.

## Operadores de membros de classe: indexação de objetos agregados
    var letters = "letters".ToCharArray();
    char letter = letters[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example we take the second character from the array
    //by calling letters[1]
    //NB: Array Indexing starts at 0; i.e. the first letter would be given by letters[0].

## Operadores binários com atribuição
C# tem vários operadores que podem ser combinados com um sinal `=` para avaliar o resultado do operador e então atribuir o resultado à variável original.

Exemplo:

    x += y

é o mesmo que

    x = x + y

Operadores de atribuição:

 - `+=`
 - `-=`
 - `*=`
 - `/=`
 - `%=`
 - `&=`
 - `|=`
 - `^=`
 - `<<=`
 - `>>=`

## tipo de
Obtém o objeto `System.Type` para um tipo.
     
    System.Type type = typeof(Point)        //System.Drawing.Point      
    System.Type type = typeof(IDisposable)  //System.IDisposable
    System.Type type = typeof(Colors)       //System.Drawing.Color
    System.Type type = typeof(List<>)       //System.Collections.Generic.List`1[T]

Para obter o tipo de tempo de execução, use o método `GetType` para obter o `System.Type` da instância atual.

O operador `typeof` recebe um nome de tipo como parâmetro, que é especificado em tempo de compilação.

    public class Animal {} 
    public class Dog : Animal {}
    
    var animal = new Dog();

    Assert.IsTrue(animal.GetType() == typeof(Animal)); // fail, animal is typeof(Dog) 
    Assert.IsTrue(animal.GetType() == typeof(Dog));    // pass, animal is typeof(Dog)
    Assert.IsTrue(animal is Animal);                   // pass, animal implements Animal


## operador nomedo
Retorna uma string que representa o nome não qualificado de uma `variável`, `tipo` ou `membro`.

    int counter = 10;
    nameof(counter); // Returns "counter"
    Client client = new Client();
    nameof(client.Address.PostalCode)); // Returns "PostalCode"

O operador `nameof` foi introduzido no C# 6.0. Ele é avaliado em tempo de compilação e o valor da string retornado é inserido em linha pelo compilador, então pode ser usado na maioria dos casos onde a string constante pode ser usada (por exemplo, os rótulos `case` em uma instrução `switch`, atributos , etc...). Pode ser útil em casos como gerar e registrar exceções, atributos, links de ação MVC, etc...

