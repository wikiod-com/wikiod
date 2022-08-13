---
title: "Diccionarios"
slug: "diccionarios"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

## Inicializar un diccionario con un inicializador de colección
    // Translates to `dict.Add(1, "First")` etc.
    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

    // Translates to `dict[1] = "First"` etc.
    // Works in C# 6.0.
    var dict = new Dictionary<int, string>()
    {
        [1] = "First",
        [2] = "Second",
        [3] = "Third"
    };


## Agregar a un diccionario
    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict.Add(1, "First");
    dict.Add(2, "Second");
   
    // To safely add items (check to ensure item does not already exist - would throw)
    if(!dict.ContainsKey(3))
    {
       dict.Add(3, "Third");
    }

Alternativamente, se pueden agregar/establecer a través de un indexador. (Un indexador internamente parece una propiedad, tiene un get y un set, pero toma un parámetro de cualquier tipo que se especifica entre corchetes):

    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict[1] = "First";
    dict[2] = "Second";
    dict[3] = "Third";

A diferencia del método `Add` que arroja una excepción, si una clave ya está contenida en el diccionario, el indexador simplemente reemplaza el valor existente.

Para un diccionario seguro para subprocesos, use `ConcurrentDictionary<TKey, TValue>`:

    var dict = new ConcurrentDictionary<int, string>();
    dict.AddOrUpdate(1, "First", (oldKey, oldValue) => "First");



## Obtener un valor de un diccionario
Dado este código de configuración:

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Es posible que desee leer el valor de la entrada con la clave 1. Si la clave no existe, obtener un valor generará una `KeyNotFoundException`, por lo que es posible que desee verificar primero con `ContainsKey`:

    if (dict.ContainsKey(1))
        Console.WriteLine(dict[1]);

Esto tiene una desventaja: buscará en su diccionario dos veces (una para verificar la existencia y otra para leer el valor). Para un diccionario grande, esto puede afectar el rendimiento. Afortunadamente ambas operaciones se pueden realizar juntas:

    string value;
    if (dict.TryGetValue(1, out value))
        Console.WriteLine(value);


## Cree un Diccionario<cadena, T> con teclas que no distinguen entre mayúsculas y minúsculas.
    var MyDict = new Dictionary<string,T>(StringComparison.InvariantCultureIgnoreCase)

## Enumerar un diccionario
Puede enumerar a través de un diccionario en una de las 3 formas:

**Usando pares de KeyValue**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(KeyValuePair<int, string> kvp in dict) 
    {
       Console.WriteLine("Key : " + kvp.Key.ToString() + ", Value : " + kvp.Value);
    }

**Uso de claves**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(int key in dict.Keys)
    {
        Console.WriteLine("Key : " + key.ToString() + ", Value : " + dict[key]);
    }

**Uso de valores**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(string s in dict.Values)
    {
        Console.WriteLine("Value : " + s);
    }

## Diccionario Concurrente<TKey, TValue> (desde .NET 4.0)
> Representa una colección segura para subprocesos de pares clave/valor que se pueden
> accedido por múltiples subprocesos al mismo tiempo.

Crear una instancia
--------------------

La creación de una instancia funciona de la misma manera que con ```Dictionary<TKey, TValue>```, por ejemplo:

    var dict = new ConcurrentDictionary<int, string>();

Agregar o actualizar
------------------

Puede que se sorprenda de que no haya un método `Add`, sino `AddOrUpdate` con 2 sobrecargas:

(1) `AddOrUpdate(TKey key, TValue, Func<TKey, TValue, TValue> addValue)` - *Agrega un par clave/valor si la clave aún no existe, o actualiza un par clave/valor usando la función especificada si la clave ya existe.*

(2) `AddOrUpdate(TKey key, Func<TKey, TValue> addValue, Func<TKey, TValue, TValue> updateValueFactory)` - *Utiliza las funciones especificadas para agregar un par clave/valor si la clave aún no existe o para actualizar un par clave/valor si la clave ya existe.*

Agregar o actualizar un valor, sin importar cuál era el valor si ya estaba presente para la clave dada (1):

    string addedValue = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => "First");

Agregar o actualizar un valor, pero ahora modificando el valor en la actualización, en función del valor anterior (1):

    string addedValue2 = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => $"{valueOld} Updated");

Usando la sobrecarga (2) también podemos agregar un nuevo valor usando una fábrica:

    string addedValue3 = dict.AddOrUpdate(1, (key) => key == 1 ? "First" : "Not First", (updateKey, valueOld) => $"{valueOld} Updated");

Obtener valor
-----------------
Obtener un valor es lo mismo que con `Dictionary<TKey,TValue>`:

    string value = null;
    bool success = dict.TryGetValue(1, out value);

Obtener o agregar un valor
-------------------------
Hay dos sobrecargas de métodos, que **obtendrán o agregarán** un valor de manera segura para subprocesos.

Obtenga el valor con la clave 2, o agregue el valor "Segundo" si la clave no está presente:

    string theValue = dict.GetOrAdd(2, "Second");

Usando una fábrica para agregar un valor, si el valor no está presente:

    string theValue2 = dict.GetOrAdd(2, (key) => key == 2 ? "Second" : "Not Second." );




## IEnumerable a Diccionario (≥ .NET 3.5)
Cree un [Diccionario&lt;TKey, TValue&gt;][1] a partir de un [IEnumerable&lt;T&gt;][2]:

    using System;
    using System.Collections.Generic;
    using System.Linq;

<b></b>

    public class Fruits
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

<b></b>

    var fruits = new[]
    { 
        new Fruits { Id = 8 , Name = "Apple" },
        new Fruits { Id = 3 , Name = "Banana" },
        new Fruits { Id = 7 , Name = "Mango" },
    };

    
    // Dictionary<int, string>                  key      value
    var dictionary = fruits.ToDictionary(x => x.Id, x => x.Name);

[1]: https://msdn.microsoft.com/en-us/library/xfhwa508(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/9eekhta0(v=vs.100).aspx



## Eliminación de un diccionario
Dado este código de configuración:

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Utilice el método `Remove` para eliminar una clave y su valor asociado.

    bool wasRemoved = dict.Remove(2);

Ejecutar este código elimina la clave `2` y su valor del diccionario. `Remove` devuelve un valor booleano que indica si la clave especificada se encontró y eliminó del diccionario. Si la clave no existe en el diccionario, no se elimina nada del diccionario y se devuelve falso (no se lanza ninguna excepción).

Es **incorrecto** intentar eliminar una clave estableciendo el valor de la clave en `null`.

    dict[2] = null; // WRONG WAY TO REMOVE!

Esto no quitará la llave. Simplemente reemplazará el valor anterior con un valor de `null`.

Para eliminar todas las claves y valores de un diccionario, utilice el método `Borrar`.

    dict.Clear();

Después de ejecutar `Clear`, el `Count` del diccionario será 0, pero la capacidad interna permanecerá sin cambios.

## Contiene la clave (TKey)
Para comprobar si un `Diccionario` tiene una clave específica, puede llamar al método [`ContainsKey(TKey)`][1] y proporcionar la clave del tipo `TKey`. El método devuelve un valor `bool` cuando la clave existe en el diccionario. Para muestra:

    var dictionary = new Dictionary<string, Customer>()
    {
       {"F1", new Customer() { FirstName = "Felipe", ... } },
       {"C2", new Customer() { FirstName = "Carl", ... } },
       {"J7", new Customer() { FirstName = "John", ... } },
       {"M5", new Customer() { FirstName = "Mary", ... } },
    };

Y verifique si existe un `C2` en el Diccionario:

    if (dictionary.ContainsKey("C2")) 
    {
       // exists
    }

El método containskey está disponible en la versión genérica [`Dictionary<TKey, TValue>`][1].


[1]: https://msdn.microsoft.com/library/htszx2dy(v=vs.110).aspx

## Diccionario a lista
Crear una lista de KeyValuePair:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<KeyValuePair<int, int>> list = new List<KeyValuePair<int, int>>();
    list.AddRange(dictionary);

Creación de una lista de claves:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Keys);

Crear una lista de valores:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Values);


## ConcurrentDictionary aumentado con Lazy'1 reduce el cálculo duplicado
## Problema

ConcurrentDictionary brilla cuando se trata de devolver instantáneamente las claves existentes de la memoria caché, en su mayoría sin bloqueo y compitiendo a nivel granular.
Pero, ¿qué sucede si la creación del objeto es realmente costosa, supera el costo del cambio de contexto y se producen algunas fallas en la memoria caché?

Si se solicita la misma clave de varios subprocesos, uno de los objetos resultantes de las operaciones en conflicto se agregará finalmente a la colección y los demás se desecharán, desperdiciando el recurso de la CPU para crear el objeto y el recurso de memoria para almacenar el objeto temporalmente. . También se podrían desperdiciar otros recursos. Esto es realmente malo.

## Solución

Podemos combinar `ConcurrentDictionary<TKey, TValue>` con `Lazy<TValue>`. La idea es que el método ConcurrentDictionary GetOrAdd solo pueda devolver el valor que realmente se agregó a la colección. Los objetos perezosos perdidos también podrían desperdiciarse en este caso, pero eso no es un gran problema, ya que el objeto Lazy en sí es relativamente económico. La propiedad Value del perdedor Lazy nunca se solicita, porque somos inteligentes al solicitar solo la propiedad Value del que realmente se agregó a la colección, el que devuelve el método GetOrAdd:

    public static class ConcurrentDictionaryExtensions
    {
        public static TValue GetOrCreateLazy<TKey, TValue>(
            this ConcurrentDictionary<TKey, Lazy<TValue>> d,
            TKey key,
            Func<TKey, TValue> factory)
        {
            return
                d.GetOrAdd(
                    key,
                    key1 =>
                        new Lazy<TValue>(() => factory(key1),
                        LazyThreadSafetyMode.ExecutionAndPublication)).Value;
        }
    }

El almacenamiento en caché de objetos XmlSerializer puede ser particularmente costoso y también hay mucha controversia al iniciar la aplicación. Y hay más en esto: si se trata de serializadores personalizados, también habrá una pérdida de memoria durante el resto del ciclo de vida del proceso. El único beneficio de ConcurrentDictionary en este caso es que durante el resto del ciclo de vida del proceso no habrá bloqueos, pero el inicio de la aplicación y el uso de la memoria serían inaceptables. Este es un trabajo para nuestro ConcurrentDictionary, aumentado con Lazy:

    private ConcurrentDictionary<Type, Lazy<XmlSerializer>> _serializers =
        new ConcurrentDictionary<Type, Lazy<XmlSerializer>>();
    
    public XmlSerializer GetSerialier(Type t)
    {
        return _serializers.GetOrCreateLazy(t, BuildSerializer);
    }
    
    private XmlSerializer BuildSerializer(Type t)
    {
        throw new NotImplementedException("and this is a homework");
    }

