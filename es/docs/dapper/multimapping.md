---
title: "multimapeo"
slug: "multimapeo"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Sintaxis
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TReturn>(
            this IDbConnection cnn, string sql, Func<TFirst, TSecond, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)` 
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TTird, TFourth, TFifth, TSixth, TSeventh, TReturn>(this IDbConnection cnn, string sql, Func<TFirst, TSecond, TTird, TFourth, TFifth, TSixth, TSeventh, TReturn> map, object param = null, IDbTransaction transacción = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)`
- `public static IEnumerable<TReturn> Query<TReturn>(este IDbConnection cnn, string sql, Type[] tipos, Func<object[], TReturn> map, object param = null, IDbTransaction transacción = null, bool buffered = true, cadena splitOn = "Id", int? commandTimeout = nulo, CommandType? commandType = nulo)
        `

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| CNN | Su conexión a la base de datos, que ya debe estar abierta. |
| sql | Comando a ejecutar.|
| tipos | Matriz de tipos en el conjunto de registros. |
| mapa | `Func<>` que maneja la construcción del resultado devuelto. |
| parámetro | Objeto del que extraer parámetros. |
| transacción | Transacción de la que forma parte esta consulta, si corresponde. |
| amortiguado | Si almacenar o no en el búfer la lectura de los resultados de la consulta. Este es un parámetro opcional y el valor predeterminado es verdadero. Cuando buffered es verdadero, los resultados se almacenan en un `List<T>` y luego se devuelven como un `IEnumerable<T>` que es seguro para la enumeración múltiple. Cuando buffered es falso, la conexión sql se mantiene abierta hasta que termine de leer, lo que le permite procesar una sola fila a la vez en la memoria. Múltiples enumeraciones generarán conexiones adicionales a la base de datos. Mientras que el falso almacenado en búfer es altamente eficiente para reducir el uso de la memoria si solo mantiene fragmentos muy pequeños de los registros devueltos, tiene una [sobrecarga de rendimiento considerable] (http://stackoverflow.com/a/30493725/37055) en comparación con la materialización ansiosa del resultado. establecer. Por último, si tiene numerosas conexiones sql simultáneas sin búfer, debe considerar la inanición del grupo de conexiones, lo que hace que las solicitudes se bloqueen hasta que las conexiones estén disponibles. |
| dividir en | El campo del que debemos dividir y leer el segundo objeto (predeterminado: id). Puede ser una lista delimitada por comas cuando hay más de un tipo en un registro. |
| comandoTiempo de espera | Número de segundos antes del tiempo de espera de ejecución del comando. |
| tipo de comando | ¿Es un proceso almacenado o un lote? |

## Mapeo simple de varias tablas
Digamos que tenemos una consulta de los jinetes restantes que necesita llenar una clase de Persona.

| Nombre | nacido | Residencia |
|-----------------|------|------------------------ --|
| Daniel Dennet | 1942 | Estados Unidos de América |
| Sam Harris | 1967 | Estados Unidos de América |
| Richard Dawkins | 1941 | Reino Unido |

    public class Person
    {
        public string Name { get; set; }
        public int Born { get; set; }
        public Country Residience { get; set; }
    }

    public class Country
    {
        public string Residence { get; set; }
    }

Podemos llenar la clase de persona así como la propiedad Residence con una instancia de Country usando una `Query<>` de sobrecarga que toma una `Func<>` que se puede usar para componer la instancia devuelta. `Func<>` puede tomar hasta 7 tipos de entrada con el argumento genérico final siempre siendo el tipo de retorno.

    var sql = @"SELECT 'Daniel Dennett' AS Name, 1942 AS Born, 'United States of America' AS Residence
    UNION ALL SELECT 'Sam Harris' AS Name, 1967 AS Born, 'United States of America' AS Residence
    UNION ALL SELECT 'Richard Dawkins' AS Name, 1941 AS Born, 'United Kingdom' AS Residence";

    var result = connection.Query<Person, Country, Person>(sql, (person, country) => {
            if(country == null)
            {
                country = new Country { Residence = "" };
            }
            person.Residience = country;
            return person;
        }, 
        splitOn: "Residence");

> Tenga en cuenta el uso del argumento `splitOn: "Residence"`, que es la primera columna del siguiente tipo de clase que se completará (en este caso, `País`). Dapper buscará automáticamente una columna llamada *Id* para dividir, pero si no encuentra una y no se proporciona `splitOn`, se lanzará una `System.ArgumentException` con un mensaje útil. Entonces, aunque es opcional, generalmente deberá proporcionar un valor `splitOn`.


## Mapeo de uno a muchos
Veamos un ejemplo más complejo que contiene una relación de uno a muchos. Nuestra consulta ahora contendrá varias filas con datos duplicados y tendremos que manejar esto. Hacemos esto con una búsqueda en un cierre.

La consulta cambia ligeramente al igual que las clases de ejemplo.

| identificación | Nombre | nacido | PaísId | Nombre del país | ID del libro | Nombre del libro |
|----|-----------------|------|-----------|------- ------------------|--------|------------------------------------ ---------------------------------|
| 1 | Daniel Dennet | 1942 | 1 | Estados Unidos de América | 1 | Lluvias de ideas |
| 1 | Daniel Dennet | 1942 | 1 | Estados Unidos de América | 2 | Sala de codo |
| 2 | Sam Harris | 1967 | 1 | Estados Unidos de América | 3 | El paisaje moral |
| 2 | Sam Harris | 1967 | 1 | Estados Unidos de América | 4 | despertar: una guía para la espiritualidad sin religión |
| 3 | Richard Dawkins | 1941 | 2 | Reino Unido | 5 | La magia de la realidad: cómo sabemos qué es realmente cierto |
| 3 | Richard Dawkins | 1941 | 2 | Reino Unido | 6 | Un apetito por la maravilla: la formación de un científico |


    public class Person
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int Born { get; set; }
        public Country Residience { get; set; }
        public ICollection<Book> Books { get; set; }
    }

    public class Country
    {
        public int CountryId { get; set; }
        public string CountryName { get; set; }
    }

    public class Book
    {
        public int BookId { get; set; }
        public string BookName { get; set; }
    }

El diccionario `remainingHorsemen` se completará con instancias totalmente materializadas de los objetos de persona. Para cada fila del resultado de la consulta, se pasan los valores asignados de instancias de los tipos definidos en los argumentos lambda y depende de usted cómo manejar esto.
              
                var sql = @"SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 1 AS BookId, 'Brainstorms' AS BookName
    UNION ALL SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 2 AS BookId, 'Elbow Room' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 3 AS BookId, 'The Moral Landscape' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 4 AS BookId, 'Waking Up: A Guide to Spirituality Without Religion' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 5 AS BookId, 'The Magic of Reality: How We Know What`s Really True' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 6 AS BookId, 'An Appetite for Wonder: The Making of a Scientist' AS BookName";

    var remainingHorsemen = new Dictionary<int, Person>();
    connection.Query<Person, Country, Book, Person>(sql, (person, country, book) => {
        //person
        Person personEntity;
        //trip
        if (!remainingHorsemen.TryGetValue(person.Id, out personEntity))
        {
            remainingHorsemen.Add(person.Id, personEntity = person);
        }
    
        //country
        if(personEntity.Residience == null)
        {
            if (country == null)
            {
                country = new Country { CountryName = "" };
            }
            personEntity.Residience = country;
        }                    
    
        //books
        if(personEntity.Books == null)
        {
            personEntity.Books = new List<Book>();
        }
    
        if (book != null)
        {
            if (!personEntity.Books.Any(x => x.BookId == book.BookId))
            {
                personEntity.Books.Add(book);
            }
        }
    
        return personEntity;
    }, 
    splitOn: "CountryId,BookId");

> Note cómo el argumento `splitOn` es una lista delimitada por comas de las primeras columnas del siguiente tipo.


## Asignaciones personalizadas
Si los nombres de las columnas de consulta no coinciden con sus clases, puede configurar asignaciones para tipos. Este ejemplo demuestra el mapeo usando `System.Data.Linq.Mapping.ColumnAttribute` así como un mapeo personalizado.

> Las asignaciones solo deben configurarse una vez por tipo, así que configúrelas al iniciar la aplicación o en otro lugar donde solo se inicialicen una vez.

Asumiendo la misma consulta que el ejemplo de uno a muchos nuevamente y las clases refactorizadas hacia mejores nombres así:


    public class Person
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int Born { get; set; }
        public Country Residience { get; set; }
        public ICollection<Book> Books { get; set; }
    }

    public class Country
    {
        [System.Data.Linq.Mapping.Column(Name = "CountryId")]
        public int Id { get; set; }

        [System.Data.Linq.Mapping.Column(Name = "CountryName")]
        public string Name { get; set; }
    }

    public class Book
    {
        public int Id { get; set; }

        public string Name { get; set; }
    }

>Observe cómo `Book` no se basa en `ColumnAttribute` pero necesitaríamos mantener la declaración `if`

Ahora coloque este código de mapeo en algún lugar de su aplicación donde solo se ejecute una vez:

    Dapper.SqlMapper.SetTypeMap(
        typeof(Country),
        new CustomPropertyTypeMap(
            typeof(Country),
            (type, columnName) =>
                type.GetProperties().FirstOrDefault(prop =>
                    prop.GetCustomAttributes(false)
                        .OfType<System.Data.Linq.Mapping.ColumnAttribute>()
                        .Any(attr => attr.Name == columnName)))
    );


    var bookMap = new CustomPropertyTypeMap(
        typeof(Book),
        (type, columnName) =>
        {
            if(columnName == "BookId")
            {
                return type.GetProperty("Id");
            }

            if (columnName == "BookName")
            {
                return type.GetProperty("Name");
            }

            throw new InvalidOperationException($"No matching mapping for {columnName}");
        }        
    );
    Dapper.SqlMapper.SetTypeMap(typeof(Book), bookMap);

Luego, la consulta se ejecuta usando cualquiera de los ejemplos `Query<>` anteriores.

En [esta respuesta][1] se muestra una forma más sencilla de agregar las asignaciones.


[1]: http://stackoverflow.com/a/12615036/2613363

## Mapeo de más de 7 tipos
A veces, la cantidad de tipos que está mapeando excede los 7 proporcionados por Func<> que hace la construcción.

En lugar de usar `Query<>` con las entradas de argumento de tipo genérico, proporcionaremos los tipos para mapear como una matriz, seguida de la función de mapeo. Aparte de la configuración manual inicial y la conversión de los valores, el resto de la función no cambia.

              
                var sql = @"SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 1 AS BookId, 'Brainstorms' AS BookName
    UNION ALL SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 2 AS BookId, 'Elbow Room' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 3 AS BookId, 'The Moral Landscape' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 4 AS BookId, 'Waking Up: A Guide to Spirituality Without Religion' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 5 AS BookId, 'The Magic of Reality: How We Know What`s Really True' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 6 AS BookId, 'An Appetite for Wonder: The Making of a Scientist' AS BookName";

    var remainingHorsemen = new Dictionary<int, Person>();
    connection.Query<Person>(sql,
        new[]
        {
            typeof(Person),
            typeof(Country),
            typeof(Book)
        }
        , obj => {
    
            Person person = obj[0] as Person;
            Country country = obj[1] as Country;
            Book book = obj[2] as Book;
    
            //person
            Person personEntity;
            //trip
            if (!remainingHorsemen.TryGetValue(person.Id, out personEntity))
            {
                remainingHorsemen.Add(person.Id, personEntity = person);
            }
    
            //country
            if(personEntity.Residience == null)
            {
                if (country == null)
                {
                    country = new Country { CountryName = "" };
                }
                personEntity.Residience = country;
            }                    
    
            //books
            if(personEntity.Books == null)
            {
                personEntity.Books = new List<Book>();
            }
    
            if (book != null)
            {
                if (!personEntity.Books.Any(x => x.BookId == book.BookId))
                {
                    personEntity.Books.Add(book);
                }
            }
    
            return personEntity;
    },
    splitOn: "CountryId,BookId");




