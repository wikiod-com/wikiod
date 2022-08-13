---
title: "Operador de fusión nula"
slug: "operador-de-fusion-nula"
draft: false
images: []
weight: 9486
type: docs
toc: true
---

## Sintaxis
- var resultado = posibleNullObject ?? valor por defecto;

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |
| `PosibleNullObject` | El valor para probar el valor nulo. Si no es nulo, se devuelve este valor. Debe ser un tipo anulable. |
| `valor predeterminado` | El valor devuelto si `possibleNullObject` es nulo. Debe ser del mismo tipo que `possibleNullObject`. |

El propio operador de fusión nula son dos signos de interrogación consecutivos: `??`

Es una abreviatura de la expresión condicional:

    possibleNullObject != null ? possibleNullObject : defaultValue

El operando del lado izquierdo (objeto que se está probando) debe ser un tipo de valor que acepta valores NULL o un tipo de referencia, o se producirá un error de compilación.

Los ?? El operador funciona tanto para tipos de referencia como para tipos de valor.



## Uso básico
El uso del [`operador coalescente nulo (??)`][2] le permite especificar un valor predeterminado para un tipo anulable si el operando de la izquierda es `nulo`.

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString ?? "not provided"));

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/GNosPU)

Esto es lógicamente equivalente a:

    string testString = null;
    if (testString == null)
    {
        Console.WriteLine("The specified string is - not provided");
    }
    else
    {
        Console.WriteLine("The specified string is - " + testString);
    }

o usando el operador [operador ternario (?:)][1]:

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString == null ? "not provided" : testString));


[1]: https://www.wikiod.com/es/docs/c%23/18/operators/6029/ternary-operator#t=201610101110242934481
[2]: https://msdn.microsoft.com/en-us/library/ms173224.aspx

## Fallo nulo y encadenamiento
El operando de la izquierda debe ser anulable, mientras que el operando de la derecha puede o no serlo. El resultado se escribirá en consecuencia.

**No anulable**

    int? a = null;
    int b = 3;
    var output = a ?? b;
    var type = output.GetType();  

    Console.WriteLine($"Output Type :{type}");
    Console.WriteLine($"Output value :{output}");

**Producción:**
>Escriba :Sistema.Int32
>valor :3

[Ver demostración][1]

**Anulable**

    int? a = null;
    int? b = null;
    var output = a ?? b;

`output` será del tipo `int?` e igual a `b`, o `null`.

**Coalescencia Múltiple**

La coalescencia también se puede hacer en cadenas:

    int? a = null;
    int? b = null;
    int c = 3;
    var output = a ?? b ?? c;

    var type = output.GetType();    
    Console.WriteLine($"Type :{type}");
    Console.WriteLine($"value :{output}");

**Producción:**
>Escriba :Sistema.Int32
> valor :3

[Ver demostración][2]

**Encadenamiento condicional nulo**

El operador coalescente nulo se puede utilizar junto con el [operador de propagación nula][3] para proporcionar un acceso más seguro a las propiedades de los objetos.

    object o = null;
    var output = o?.ToString() ?? "Default Value";

**Producción:**
>Tipo :Sistema.Cadena
> valor: valor predeterminado

[Ver demostración][4]


[1]: https://dotnetfiddle.net/hKHOcN
[2]: https://dotnetfiddle.net/xC8Bmc
[3]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607280322338995462
[4]: https://dotnetfiddle.net/nk1QRn

## Operador coalescente nulo con llamadas a métodos
El operador coalescente nulo hace que sea fácil garantizar que un método que puede devolver "null" volverá a un valor predeterminado.

Sin el operador coalescente nulo:

    string name = GetName();

    if (name == null)
        name = "Unknown!";

Con el operador coalescente nulo:

    string name = GetName() ?? "Unknown!";


## Usar lo existente o crear uno nuevo
Un escenario de uso común en el que esta característica realmente ayuda es cuando está buscando un objeto en una colección y necesita crear uno nuevo si aún no existe.

    IEnumerable<MyClass> myList = GetMyList();
    var item = myList.SingleOrDefault(x => x.Id == 2) ?? new MyClass { Id = 2 };

## Inicialización de propiedades diferidas con operador de fusión nulo
    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars
    {
        get { return _fooBars ?? (_fooBars = new List<FooBar>()); }
    }

La primera vez que se accede a la propiedad `.FooBars`, la variable `_fooBars` se evaluará como `nula`, por lo que la declaración de asignación asigna y evalúa el valor resultante.

seguridad del hilo
===
Esta es una forma **no segura para subprocesos** de implementar propiedades perezosas. Para la pereza segura para subprocesos, use la clase [`Lazy<T>`][1] integrada en .NET Framework.

Azúcar sintáctico C# 6 usando cuerpos de expresión
====

Tenga en cuenta que desde C# 6, esta sintaxis se puede simplificar utilizando el cuerpo de expresión para la propiedad:

    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars => _fooBars ?? ( _fooBars = new List<FooBar>() );

Los accesos posteriores a la propiedad arrojarán el valor almacenado en la variable `_fooBars`.

Ejemplo en el patrón MVVM
===

Esto se usa a menudo cuando se implementan comandos en el patrón MVVM. En lugar de inicializar los comandos con entusiasmo con la construcción de un modelo de vista, los comandos se inicializan de forma perezosa utilizando este patrón de la siguiente manera:

    private ICommand _actionCommand = null;
    public ICommand ActionCommand =>
       _actionCommand ?? ( _actionCommand = new DelegateCommand( DoAction ) );


[1]: https://www.wikiod.com/es/docs/c%23/1192/singleton-implementation/6795/lazy-thread-safe-singleton-using-lazyt

