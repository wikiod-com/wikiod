---
title: "Operadores condicionales nulos"
slug: "operadores-condicionales-nulos"
draft: false
images: []
weight: 9614
type: docs
toc: true
---

## Sintaxis
- X?.Y; // nulo si X es nulo si no X.Y
- X?.Y?.Z; // nulo si X es nulo o Y es nulo si no X.Y.Z
- X?[índice]; // nulo si X es nulo si no X [índice]
- X?.ValueMethod(); // nulo si X es nulo de lo contrario el resultado de X.ValueMethod();
- X?.VoidMethod(); //no hacer nada si X es nulo; de lo contrario, llamar a X.VoidMethod();


Tenga en cuenta que al usar el operador de fusión nulo en un tipo de valor `T` obtendrá un `Nullable<T>` de vuelta.

## Operador condicional nulo
El operador `?.` es azúcar sintáctico para evitar verificaciones nulas detalladas. También se conoce como [Operador de navegación segura] (https://en.wikipedia.org/wiki/Safe_Navigation_Operator).

------

Clase utilizada en el siguiente ejemplo:

    public class Person
    {
        public int Age { get; set; }
        public string Name { get; set; }
        public Person Spouse { get; set; }
    }

Si un objeto es potencialmente nulo (como una función que devuelve un tipo de referencia), primero se debe verificar si el objeto es nulo para evitar una posible `NullReferenceException`. Sin el operador condicional nulo, esto se vería así:

    Person person = GetPerson();

    int? age = null;
    if (person != null)
        age = person.Age;

El mismo ejemplo usando el operador condicional nulo:

    Person person = GetPerson();

    var age = person?.Age;    // 'age' will be of type 'int?', even if 'person' is not null


----------


Encadenamiento del operador
---------------------
El operador condicional nulo se puede combinar en los miembros y submiembros de un objeto.

    // Will be null if either `person` or `person.Spouse` are null
    int? spouseAge = person?.Spouse?.Age;


----------


Combinando con el Operador Nulo-Coalescente
---------------------
El operador condicional nulo se puede combinar con el [operador de fusión nula][1] para proporcionar un valor predeterminado:

    // spouseDisplayName will be "N/A" if person, Spouse, or Name is null
    var spouseDisplayName = person?.Spouse?.Name ?? "N/A";



[1]: https://www.wikiod.com/es/docs/c%23/37/null-coalescing-operator#t=201610192135170167414

## El índice condicional nulo
De manera similar al operador `?.`, el operador de índice condicional nulo verifica los valores nulos al indexar en una colección que puede ser nula.

    string item = collection?[index];

es azúcar sintáctico para

    string item = null;
    if(collection != null)
    {
        item = collection[index];
    }


## Evitar las excepciones de referencia nula
    var person = new Person
    {
        Address = null;
    };
    
    var city = person.Address.City; //throws a NullReferenceException
    var nullableCity = person.Address?.City; //returns the value of null

Este efecto se puede encadenar:

    var person = new Person
    {
        Address = new Address
        {
            State = new State
            {
                Country = null
            }
        }
    };
    
    // this will always return a value of at least "null" to be stored instead
    // of throwing a NullReferenceException
    var countryName = person?.Address?.State?.Country?.Name; 

## El operador condicional nulo se puede usar con el método de extensión
[El método de extensión puede funcionar en referencias nulas] (https://www.wikiod.com/es/docs/c%23/20/extension-methods/161/null-checking#t=201607271333507907787), pero puede usar `?.` para verificación nula de todos modos.

    public class Person 
    {
        public string Name {get; set;}
    }
    
    public static class PersonExtensions
    {
        public static int GetNameLength(this Person person)
        {
            return person == null ? -1 : person.Name.Length;
        }
    }

Normalmente, el método se activará para referencias `nulas` y devolverá -1:

    Person person = null;
    int nameLength = person.GetNameLength(); // returns -1

Al usar `?.`, el método no se activará para las referencias `nulas`, y el [tipo es `int?`](https://www.wikiod.com/es/docs/c%23/41/null-conditional-operators/ 173/operador-condicional-nulo#t=201607271345436018565):

    Person person = null;
    int? nameLength = person?.GetNameLength(); // nameLength is null.

En realidad, este comportamiento se espera por la forma en que funciona el operador `?.`: evitará realizar llamadas a métodos de instancia para instancias nulas, a fin de evitar `NullReferenceExceptions`. Sin embargo, la misma lógica se aplica al método de extensión, a pesar de la diferencia en cómo se declara el método.

Para obtener más información sobre por qué se llama al método de extensión en el primer ejemplo, consulte los [Métodos de extensión: verificación nula] (https://www.wikiod.com/es/docs/c%23/20/extension-methods/161/null- comprobando#t=201607271333507907787) documentación.

