---
title: "Operadores"
slug: "operadores"
draft: false
images: []
weight: 9543
type: docs
toc: true
---

En C#, un [operador](https://docs.microsoft.com/en-us/dotnet/csharp/program) es un elemento de programa que se aplica a uno o más operandos en una expresión o declaración. Los operadores que toman un operando, como el operador de incremento (++) o nuevo, se conocen como operadores unarios. Los operadores que toman dos operandos, como los operadores aritméticos (+,-,*,/), se denominan operadores binarios. Un operador, el operador condicional (?:), toma tres operandos y es el único operador ternario en C#.



## Sintaxis
- operador OperandType estático público operatorSymbol (OperandType operand1)
- operador OperandType estático público operatorSymbol (OperandType operando1, OperandoTipo2 operando2)

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| símbolo del operador | El operador está sobrecargado, p. +, -, /, * |
| TipoOperando | El tipo que devolverá el operador sobrecargado.
| operando1 | El primer operando que se utilizará para realizar la operación.
| operando2 | El segundo operando que se utilizará para realizar la operación, al realizar operaciones binarias.
| declaraciones | Código opcional necesario para realizar la operación antes de devolver el resultado.

Todos los operadores se definen como `métodos estáticos` y no son `virtuales` y no se heredan.
### Precedencia de operadores

Todos los operadores tienen una "precedencia" particular según el grupo al que pertenezca el operador (los operadores del mismo grupo tienen la misma precedencia). Lo que significa que algunos operadores se aplicarán antes que otros. Lo que sigue es una lista de grupos (que contienen sus respectivos operadores) ordenados por precedencia (el más alto primero):

* **Operadores principales**
* `a.b` - Acceso de miembro.
* `a?.b` - Acceso de miembro condicional nulo.
* `->` - Eliminación de referencias de puntero combinada con acceso a miembros.
* `f(x)` - Invocación de función.
* `a[x]` - Indexador.
* `a?[x]` - Indexador condicional nulo.
* `x++` - Incremento de sufijo.
* `x--` - Decremento de postfijo.
* `nuevo` - Tipo instanciación.
* `default(T)` - Devuelve el valor inicializado predeterminado del tipo `T`.
* `typeof` - Devuelve el objeto `Type` del operando.
* `checked` - Habilita la comprobación de desbordamiento numérico.
* `unchecked`: deshabilita la verificación de desbordamiento numérico.
* `delegate`: declara y devuelve una instancia de delegado.
* `sizeof` - Devuelve el tamaño en bytes del tipo de operando.

* **Operadores unarios**
* `+x` - Devuelve `x`.
* `-x` - Negación numérica.
* `!x` - Negación lógica.
* `~x` - Bitwise complement/declares destructors.
* `++x` - Incremento de prefijo.
* `--x` - Decremento de prefijo.
* `(T)x` - Tipo de conversión.
* `await` - Espera una `Tarea`.
* `&x` - Devuelve la dirección (puntero) de `x`.
* `*x` - Eliminación de referencias de punteros.

* **Operadores multiplicativos**
* `x * y` - Multiplicación.
* `x/y` - División.
* `x % y` - Módulo.

* **Operadores aditivos**
* `x + y` - Suma.
* `x – y` - Resta.

* **Operadores de desplazamiento bit a bit**
* `x << y` - Desplazar bits a la izquierda.
* `x >> y` - Desplazar bits a la derecha.

* **Operadores relacionales/de prueba de tipo**
* `x < y` - Menor que.
* `x > y` - Mayor que.
* `x <= y` - Menor o igual que.
* `x >= y` - Mayor o igual que.
* `is` - Compatibilidad de tipos.
* `as` - Tipo de conversión.

* **Operadores de igualdad**
* `x == y` - Igualdad.
* `x != y` - No es igual.

* **Operador AND lógico**
* `x & y` - AND lógico/bit a bit.

* **Operador XOR lógico**
* `x ^ y` - XOR lógico/bit a bit.

* **Operador lógico OR**
* `x | y` - OR lógico/bit a bit.

* **Operador AND condicional**
* `x && y` - AND lógico en cortocircuito.

* **Operador OR condicional**
* `x || y` - OR lógico de cortocircuito.

* **Operador de fusión nula**
*`x?? y` - Devuelve `x` si no es nulo; de lo contrario, devuelve `y`.

* **Operador Condicional**
* `x? y : z` - Evalúa/devuelve `y` si `x` es verdadero; de lo contrario, evalúa `z`.


---

**Contenido relacionado**

- [Operador de fusión nula][1]
- [Operador condicional nulo][2]
- [nombre del Operador][3]

[1]: https://www.wikiod.com/es/docs/c%23/37/null-coalescing-operator#t=201511232329424573937
[2]: https://www.wikiod.com/es/docs/c%23/41/the-null-conditional-operator#t=201511232329445644147
[3]: https://www.wikiod.com/es/docs/c%23/80/nameof-operator#t=201608081725023270827

## Operadores sobrecargables
C# permite que los tipos definidos por el usuario sobrecarguen a los operadores mediante la definición de funciones miembro estáticas mediante la palabra clave `operador`.
El siguiente ejemplo ilustra una implementación del operador `+`.

Si tenemos una clase `Complejo` que representa un número complejo:

    public struct Complex
    {
        public double Real { get; set; }
        public double Imaginary { get; set; }
    }

Y queremos agregar la opción de usar el operador `+` para esta clase. es decir.:

    Complex a = new Complex() { Real = 1, Imaginary = 2 };
    Complex b = new Complex() { Real = 4, Imaginary = 8 };
    Complex c = a + b;

Tendremos que sobrecargar el operador `+` para la clase. Esto se hace usando una función estática y la palabra clave `operator`:

    public static Complex operator +(Complex c1, Complex c2)
    {
       return new Complex 
       { 
           Real = c1.Real + c2.Real,
           Imaginary = c1.Imaginary + c2.Imaginary 
       };
    }

Los operadores como `+`, `-`, `*`, `/` pueden sobrecargarse. Esto también incluye operadores que no devuelven el mismo tipo (por ejemplo, `==` y `!=` pueden sobrecargarse, a pesar de devolver valores booleanos). La siguiente regla relacionada con los pares también se aplica aquí.

Los operadores de comparación deben sobrecargarse en pares (por ejemplo, si `<` está sobrecargado, `>` también debe sobrecargarse).

Se puede ver una lista completa de operadores sobrecargables (así como de operadores no sobrecargables y las restricciones impuestas a algunos operadores sobrecargables) en [MSDN - Operadores sobrecargables (Guía de programación de C#)][1].

<!-- si la versión [gte 7.0] -->
la sobrecarga de `operator is` se introdujo con el mecanismo de coincidencia de patrones de C# 7.0. Para obtener más información, consulte [Coincidencia de patrones][2]

Dado un tipo 'Cartesiano' definido de la siguiente manera

    public class Cartesian
    {
        public int X { get; }
        public int Y { get; }
    }   

Un `operador sobrecargable es` podría, p. definirse para coordenadas `Polares`

    public static class Polar
    {
        public static bool operator is(Cartesian c, out double R, out double Theta)
        {
            R = Math.Sqrt(c.X*c.X + c.Y*c.Y);
            Theta = Math.Atan2(c.Y, c.X);
            return c.X != 0 || c.Y != 0;
        }
    }

que se puede usar así

    var c = Cartesian(3, 4);
    if (c is Polar(var R, *))
    {
        Console.WriteLine(R);
    }

(El ejemplo está tomado de la [Documentación de coincidencia de patrones de Roslyn][3])
<!-- versión final si -->

[1]: https://msdn.microsoft.com/en-us/library/8edha89s.aspx
[2]: https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/13323/pattern-matching#t=201608081959042378203
[3]: https://github.com/dotnet/roslyn/blob/future/docs/features/patterns.md

## Sobrecarga de operadores de igualdad
Sobrecargar solo operadores de igualdad no es suficiente. Bajo diferentes circunstancias, todos los siguientes pueden ser llamados:

1. `objeto.Equals` y `objeto.GetHashCode`
2. `IEquatable<T>.Equals` (opcional, permite evitar el boxeo)
3. `operator ==` y `operator !=` (opcional, permite usar operadores)

Al anular `Equals`, también se debe anular `GetHashCode`. Al implementar `Equals`, hay muchos casos especiales: comparar objetos de un tipo diferente, compararse consigo mismo, etc.

Cuando NO se anula, el método `Equals` y el operador `==` se comportan de manera diferente para clases y estructuras. Para las clases, solo se comparan las referencias, y para las estructuras, los valores de las propiedades se comparan a través de la reflexión, lo que puede afectar negativamente el rendimiento. `==` no se puede usar para comparar estructuras a menos que se anule.

En general, la operación de igualdad debe obedecer las siguientes reglas:

- No debe *lanzar excepciones*.
- Reflexividad: `A` siempre es igual a `A` (puede no ser cierto para los valores `NULL` en algunos sistemas).
- Transitividad: si 'A' es igual a 'B', y 'B' es igual a 'C', entonces 'A' es igual a 'C'.
- Si `A` es igual a `B`, entonces `A` y `B` tienen códigos hash iguales.
- Independencia del árbol de herencia: si `B` y `C` son instancias de `Class2` heredadas de `Class1`:
`Class1.Equals(A,B)` siempre debe devolver el mismo valor que la llamada a `Class2.Equals(A,B)`.
      

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

## Operadores relacionales
**Igual**

Comprueba si los operandos proporcionados (argumentos) son iguales

    "a" == "b"     // Returns false.
    "a" == "a"     // Returns true.
    1 == 0         // Returns false.
    1 == 1         // Returns true.
    false == true  // Returns false.
    false == false // Returns true.

A diferencia de Java, el operador de comparación de igualdad funciona de forma nativa con cadenas.

El operador de comparación de igualdad funcionará con operandos de diferentes tipos si existe una conversión implícita de uno a otro. Si no existe una conversión implícita adecuada, puede llamar a una conversión explícita o usar un método para convertir a un tipo compatible.

    1 == 1.0              // Returns true because there is an implicit cast from int to double.
    new Object() == 1.0   // Will not compile.
    MyStruct.AsInt() == 1 // Calls AsInt() on MyStruct and compares the resulting int with 1.

A diferencia de Visual Basic.NET, el operador de comparación de igualdad no es lo mismo que el operador de asignación de igualdad.

    var x = new Object();
    var y = new Object();
    x == y // Returns false, the operands (objects in this case) have different references.
    x == x // Returns true, both operands have the same reference.

<sup>*No debe confundirse con el operador de asignación (`=`).*</sup>

Para los tipos de valor, el operador devuelve "verdadero" si ambos operandos tienen el mismo valor.
Para los tipos de referencia, el operador devuelve `verdadero` si ambos operandos son iguales en *referencia* (no valor). Una excepción es que los objetos de cadena se compararán con la igualdad de valores.

**No es igual**

Comprueba si los operandos proporcionados *no* son iguales.

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

Este operador efectivamente devuelve el resultado opuesto al del operador de igualdad (`==`)

**Mas grande que**

Comprueba si el primer operando es mayor que el segundo operando.

    3 > 5    //Returns false.
    1 > 0    //Returns true.
    2 > 2    //Return false.
    
    var x = 10;
    var y = 15;
    x > y    //Returns false.
    y > x    //Returns true.

**Menos que**

Comprueba si el primer operando es menor que el segundo operando.

    2 < 4     //Returns true.
    1 < -3    //Returns false.
    2 < 2     //Return false.
    
    var x = 12;
    var y = 22;
    x < y    //Returns true.
    y < x    //Returns false.

**Mayor que igual a**

Comprueba si el primer operando es mayor que igual al segundo operando.

    7 >= 8    //Returns false.
    0 >= 0    //Returns true.
    
**Menor que igual a**

Comprueba si el primer operando es menor que el segundo operando.

    2 <= 4    //Returns true.
    1 <= -3    //Returns false.
    1 <= 1     //Returns true. 
    
  
  

## Operadores de conversión implícita y explícita
C# permite que los tipos definidos por el usuario controlen la asignación y la conversión mediante el uso de las palabras clave `explícita` e `implícita`. La firma del método toma la forma:

    public static <implicit/explicit> operator <ResultingType>(<SourceType> myType)

El método no puede tomar más argumentos, ni puede ser un método de instancia. Sin embargo, puede acceder a cualquier miembro privado del tipo en el que esté definido.

Un ejemplo de conversión `implícita` y `explícita`:

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

Permitiendo la siguiente sintaxis de conversión:

    var binaryImage = new BinaryImage();
    ColorImage colorImage = binaryImage; // implicit cast, note the lack of type 
    bool[] pixels = (bool[])binaryImage; // explicit cast, defining the type

Los operadores de conversión pueden funcionar en ambos sentidos, yendo *desde* su tipo y yendo *hacia* su tipo:

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

Finalmente, la palabra clave `as`, que puede estar involucrada en la conversión dentro de una jerarquía de tipos, **no** es válida en esta situación. Incluso después de definir una conversión `explícita` o `implícita`, no puede hacer:

    ColorImage cm = myBinaryImage as ColorImage;

Generará un error de compilación.

## Operadores en cortocircuito
*Por definición, los operadores booleanos de cortocircuito solo evaluarán el segundo operando si el primer operando no puede determinar el resultado general de la expresión.*

Significa que, si está utilizando el operador && como *primera condición && segunda condición*, evaluará *segunda condición* solo cuando *primera condición* sea verdadera y, por supuesto, el resultado general será verdadero solo si se evalúan tanto *primeroperando* como *segundooperando*. a la verdad Esto es útil en muchos escenarios, por ejemplo, imagine que desea verificar si su lista tiene más de tres elementos, pero también debe verificar si la lista se ha inicializado para no encontrarse con *NullReferenceException*. Puede lograr esto de la siguiente manera:

    bool hasMoreThanThreeElements = myList != null && mList.Count > 3;

*mList.Count > 3* no se verificará hasta que myList != null se cumpla.

**Y lógico**

`&&` es la contraparte de cortocircuito del operador booleano estándar AND (`&`).

    var x = true;
    var y = false;

    x && x // Returns true.
    x && y // Returns false (y is evaluated).
    y && x // Returns false (x is not evaluated).
    y && y // Returns false (right y is not evaluated).

**OR lógico**

`||` es la contraparte de cortocircuito del operador booleano estándar OR (`|`).

    var x = true;
    var y = false;

    x || x // Returns true (right x is not evaluated).
    x || y // Returns true (y is not evaluated).
    y || x // Returns true (x and y are evaluated).
    y || y // Returns false (y and y are evaluated).

**Ejemplo de uso**

    if(object != null && object.Property)
    // object.Property is never accessed if object is null, because of the short circuit.
        Action1();
    else
        Action2();

##? : Operador Ternario
Devuelve uno de dos valores según el valor de una expresión booleana.

Sintaxis:

    condition ? expression_if_true : expression_if_false;

Ejemplo:

    string name = "Frank";
    Console.WriteLine(name == "Frank" ? "The name is Frank" : "The name is not Frank");

El operador ternario es asociativo por la derecha, lo que permite el uso de expresiones ternarias compuestas. Esto se hace agregando ecuaciones ternarias adicionales en la posición verdadera o falsa de una ecuación ternaria principal. Se debe tener cuidado para garantizar la legibilidad, pero esto puede ser una abreviatura útil en algunas circunstancias.

En este ejemplo, una operación ternaria compuesta evalúa una función `clamp` y devuelve el valor actual si está dentro del rango, el valor `min` si está por debajo del rango o el valor `max` si está por encima del rango.

    light.intensity = Clamp(light.intensity, minLight, maxLight);

    public static float Clamp(float val, float min, float max)
    {
        return (val < min) ? min : (val > max) ? max : val;
    }

Los operadores ternarios también se pueden anidar, como:

    a ? b ? "a is true, b is true" : "a is true, b is false" : "a is false"
    
    // This is evaluated from left to right and can be more easily seen with parenthesis:
    
    a ? (b ? x : y) : z

    // Where the result is x if a && b, y if a && !b, and z if !a

Al escribir declaraciones ternarias compuestas, es común usar paréntesis o sangría para mejorar la legibilidad.

Los tipos de *expression_if_true* y *expression_if_false* deben ser idénticos o debe haber una conversión implícita de uno a otro.

    condition ? 3 : "Not three"; // Doesn't compile because `int` and `string` lack an implicit conversion.

    condition ? 3.ToString() : "Not three"; // OK because both possible outputs are strings.

    condition ? 3 : 3.5; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

    condition ? 3.5 : 3; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

Los requisitos de tipo y conversión también se aplican a sus propias clases.

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


## ?. (Operador condicional nulo)
<!-- si la versión [gte 6.0] -->

[Introducido en C# 6.0][1], el operador condicional nulo `?.` devolverá inmediatamente `null` si la expresión en su lado izquierdo se evalúa como `null`, en lugar de lanzar una `NullReferenceException`. Si su lado izquierdo se evalúa como un valor no `nulo`, se trata como un operador `.` normal. Tenga en cuenta que debido a que puede devolver `null`, su tipo de retorno siempre es un tipo que acepta valores NULL. Eso significa que para una estructura o tipo primitivo, se envuelve en `Nullable<T>`.

    var bar = Foo.GetBar()?.Value; // will return null if GetBar() returns null
    var baz = Foo.GetBar()?.IntegerValue; // baz will be of type Nullable<int>, i.e. int?
  
Esto es útil cuando se activan eventos. Normalmente, tendría que envolver la llamada del evento en una declaración if verificando `null` y luego generar el evento, lo que introduce la posibilidad de una condición de carrera. Usando el operador condicional nulo, esto se puede solucionar de la siguiente manera:

    event EventHandler<string> RaiseMe;
    RaiseMe?.Invoke("Event raised");

<!-- versión final si -->


[1]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607301051500162149

## tamaño de
Devuelve un `int` que contiene el tamaño de un tipo<sup>*</sup> en bytes.

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

<sup>**Solo admite ciertos tipos primitivos en contexto seguro.*</sup>

En un contexto inseguro, `sizeof` se puede usar para devolver el tamaño de otros tipos y estructuras primitivos.

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

## Operadores de miembros de clase: acceso de miembro condicional nulo
    var zipcode = myEmployee?.Address?.ZipCode;
    //returns null if the left operand is null.  
    //the above is the equivalent of:
    var zipcode = (string)null;
    if (myEmployee != null && myEmployee.Address != null)
        zipcode = myEmployee.Address.ZipCode;

## Operadores de miembros de clase: indexación condicional nula
    var letters = null;
    char? letter = letters?[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example  rather than throwing an error because letters is null
    //letter is assigned the value null

## Operador "o exclusivo"
El operador para un "o exclusivo" (para abreviar XOR) es: ^

Este operador devuelve verdadero cuando uno, pero solo uno, de los booleanos proporcionados es verdadero.

    true ^ false   // Returns true
    false ^ true   // Returns true
    false ^ false  // Returns false
    true ^ true    // Returns false

## Operadores de desplazamiento de bits
Los operadores de desplazamiento permiten a los programadores ajustar un número entero desplazando todos sus bits hacia la izquierda o hacia la derecha. El siguiente diagrama muestra el efecto de desplazar un valor a la izquierda en un dígito.

**Shift izquierdo**

    uint value = 15;              // 00001111
     
    uint doubled = value << 1;    // Result = 00011110 = 30
    uint shiftFour = value << 4;  // Result = 11110000 = 240

**Giro a la derecha**

    uint value = 240;             // 11110000
     
    uint halved = value >> 1;     // Result = 01111000 = 120
    uint shiftFour = value >> 4;  // Result = 00001111 = 15

## Operador predeterminado
Tipo de valor (donde T: estructura)
---
Los tipos de datos primitivos integrados, como `char`, `int` y `float`, así como los tipos definidos por el usuario declarados con `struct` o `enum`. Su valor por defecto es `nueva T()` :

    default(int)            // 0
    default(DateTime)       // 0001-01-01 12:00:00 AM
    default(char)           // '\0' This is the "null character", not a zero or a line break.
    default(Guid)           // 00000000-0000-0000-0000-000000000000
    default(MyStruct)       // new MyStruct()

    // Note: default of an enum is 0, and not the first *key* in that enum
    // so it could potentially fail the Enum.IsDefined test
    default(MyEnum)         // (MyEnum)0

Tipo de referencia (donde T : clase)
---

Cualquier tipo de `clase`, `interfaz`, matriz o delegado. Su valor predeterminado es `null`:

    default(object)         // null
    default(string)         // null
    default(MyClass)        // null
    default(IDisposable)    // null
    default(dynamic)        // null

## Postfijo y Prefijo incrementan y decrementan
El incremento de postfijo `X++` agregará `1` a `x`

    var x = 42;
    x++;
    Console.WriteLine(x); // 43

El decremento del sufijo `X--` restará uno

    var x = 42
    x--; 
    Console.WriteLine(x); // 41



`++x` se llama incremento de prefijo, incrementa el valor de x y luego devuelve x
mientras que `x++` devuelve el valor de x y luego incrementa

    var x = 42;
    Console.WriteLine(++x); // 43
    System.out.println(x); // 43

tiempo

    var x = 42;
    Console.WriteLine(x++); // 42
    System.out.println(x); // 43

ambos se usan comúnmente en for loop

    for(int i = 0; i < 10; i++)
    {
    }


## => Operador lambda
<!-- si versión [gte 3.0] -->

*El operador `=>` tiene la misma precedencia que el operador de asignación `=` y es asociativo por la derecha.*

Se usa para declarar expresiones lambda y también se usa ampliamente con [LINQ Queries] (https://www.wikiod.com/es/docs/c%23/68/linq-queries/4735/basics#t=201607251514251028068):

    string[] words = { "cherry", "apple", "blueberry" };

    int shortestWordLength = words.Min((string w) => w.Length); //5

Cuando se usa en extensiones o consultas LINQ, el tipo de los objetos generalmente se puede omitir, ya que el compilador lo infiere:

    int shortestWordLength = words.Min(w => w.Length); //also compiles with the same result

La forma general del operador lambda es la siguiente:

    (input parameters) => expression

Los parámetros de la expresión lambda se especifican antes del operador `=>`, y la expresión/declaración/bloque real que se ejecutará está a la derecha del operador:

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

Este operador se puede usar para definir delegados fácilmente, sin escribir un método explícito:

    delegate void TestDelegate(string s);
    
    TestDelegate myDelegate = s => Console.WriteLine(s + " World");

    myDelegate("Hello");

en vez de

    void MyMethod(string s)
    {
        Console.WriteLine(s + " World");
    }
    
    delegate void TestDelegate(string s);

    TestDelegate myDelegate = MyMethod;

    myDelegate("Hello");

<!-- versión final si -->

## Operador de asignación '='
El operador de asignación `=` establece el valor del operando de la izquierda en el valor del operando de la derecha y devuelve ese valor:

    int a = 3;     // assigns value 3 to variable a
    int b = a = 5; // first assigns value 5 to variable a, then does the same for variable b
    Console.WriteLine(a = 3 + 4); // prints 7


## ?? Operador de fusión nula
El operador Null-Coalescing `??` devolverá el lado izquierdo cuando no sea nulo. Si es nulo, devolverá el lado derecho.

    object foo = null;
    object bar = new object();
    
    var c = foo ?? bar;
    //c will be bar since foo was null

El operador `??` se puede encadenar, lo que permite eliminar las comprobaciones `si`.

    //config will be the first non-null returned.
    var config = RetrieveConfigOnMachine() ??
                 RetrieveConfigFromService() ??
                 new DefaultConfiguration();





## Operadores de miembros de clase: acceso de miembros
    var now = DateTime.UtcNow;
    //accesses member of a class.  In this case the UtcNow property.

## Operadores de miembros de clase: invocación de funciones
    var age = GetAge(dateOfBirth);
    //the above calls the function GetAge passing parameter dateOfBirth.

## Operadores de miembros de clase: indexación de objetos agregados
    var letters = "letters".ToCharArray();
    char letter = letters[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example we take the second character from the array
    //by calling letters[1]
    //NB: Array Indexing starts at 0; i.e. the first letter would be given by letters[0].

## Operadores binarios con asignación
C# tiene varios operadores que se pueden combinar con un signo `=` para evaluar el resultado del operador y luego asignar el resultado a la variable original.

Ejemplo:

    x += y

es lo mismo que

    x = x + y

Operadores de Asignación:

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
Obtiene el objeto `System.Type` para un tipo.
     
    System.Type type = typeof(Point)        //System.Drawing.Point      
    System.Type type = typeof(IDisposable)  //System.IDisposable
    System.Type type = typeof(Colors)       //System.Drawing.Color
    System.Type type = typeof(List<>)       //System.Collections.Generic.List`1[T]

Para obtener el tipo de tiempo de ejecución, use el método `GetType` para obtener el `System.Type` de la instancia actual.

El operador `typeof` toma un nombre de tipo como parámetro, que se especifica en tiempo de compilación.

    public class Animal {} 
    public class Dog : Animal {}
    
    var animal = new Dog();

    Assert.IsTrue(animal.GetType() == typeof(Animal)); // fail, animal is typeof(Dog) 
    Assert.IsTrue(animal.GetType() == typeof(Dog));    // pass, animal is typeof(Dog)
    Assert.IsTrue(animal is Animal);                   // pass, animal implements Animal


## nombre del operador
Devuelve una cadena que representa el nombre no calificado de una `variable`, `tipo` o `miembro`.

    int counter = 10;
    nameof(counter); // Returns "counter"
    Client client = new Client();
    nameof(client.Address.PostalCode)); // Returns "PostalCode"

El operador `nameof` se introdujo en C# 6.0. Se evalúa en tiempo de compilación y el compilador inserta el valor de cadena devuelto en línea, por lo que se puede usar en la mayoría de los casos en los que se puede usar la cadena constante (por ejemplo, las etiquetas `case` en una instrucción `switch`, los atributos , etc...). Puede ser útil en casos como generar y registrar excepciones, atributos, enlaces de acción MVC, etc.

