---
title: "Multicartographie"
slug: "multicartographie"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Syntaxe
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TReturn>(
            this IDbConnection cnn, string sql, Func<TFirst, TSecond, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)` 
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TTird, TFourth, TFifth, TSixth, TSeventh, TReturn>(this IDbConnection cnn, string sql, Func<TFirst, TSecond, TTird, TFourth, TFifth, TSixth, TSeventh, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)`
- `public static IEnumerable<TReturn> Query<TReturn>(this IDbConnection cnn, string sql, Type[] types, Func<object[], TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int ? commandTimeout = null, CommandType ? commandType = null)
        `

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| cnn | Votre connexion à la base de données, qui doit déjà être ouverte. |
| sql | Commande à exécuter.|
| types | Tableau de types dans le jeu d'enregistrements. |
| carte | `Func<>` qui gère la construction du résultat de retour. |
| paramètre | Objet à partir duquel extraire les paramètres. |
| opération | Transaction dont cette requête fait partie, le cas échéant. |
| tamponné | S'il faut ou non tamponner la lecture des résultats de la requête. Il s'agit d'un paramètre facultatif, la valeur par défaut étant true. Lorsque buffered est vrai, les résultats sont mis en mémoire tampon dans une `List<T>` puis renvoyés sous la forme d'un `IEnumerable<T>` qui est sûr pour une énumération multiple. Lorsque buffered est false, la connexion sql est maintenue ouverte jusqu'à ce que vous ayez fini de lire, ce qui vous permet de traiter une seule ligne à la fois en mémoire. Plusieurs énumérations généreront des connexions supplémentaires à la base de données. Alors que buffered false est très efficace pour réduire l'utilisation de la mémoire si vous ne conservez que de très petits fragments des enregistrements renvoyés, il a une [surcharge de performance importante] (http://stackoverflow.com/a/30493725/37055) par rapport à la matérialisation avide du résultat Positionner. Enfin, si vous avez de nombreuses connexions SQL simultanées sans tampon, vous devez tenir compte de la famine du pool de connexions, ce qui provoque le blocage des demandes jusqu'à ce que les connexions soient disponibles. |
| splitOn | Le champ à partir duquel nous devons diviser et lire le deuxième objet (par défaut : id). Il peut s'agir d'une liste délimitée par des virgules lorsque plusieurs types sont contenus dans un enregistrement. |
| commandTimeout | Nombre de secondes avant l'expiration du délai d'exécution de la commande. |
| type de commande | Est-ce un proc stocké ou un lot ? |

## Mappage multitable simple
Disons que nous avons une requête des cavaliers restants qui doit remplir une classe Person.

| Nom | Né | Résidence |
|----------------|------|------------------------ --|
| Daniel Dennett | 1942 | États-Unis d'Amérique |
| Sam Harris | 1967 | États-Unis d'Amérique |
| Richard Dawkins | 1941 | Royaume-Uni |

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

Nous pouvons remplir la classe person ainsi que la propriété Residence avec une instance de Country en utilisant une surcharge `Query<>` qui prend un `Func<>` qui peut être utilisé pour composer l'instance renvoyée. Le `Func<>` peut prendre jusqu'à 7 types d'entrée avec l'argument générique final étant toujours le type de retour.

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

> Notez l'utilisation de l'argument `splitOn: "Residence"` qui est la 1ère colonne du prochain type de classe à remplir (dans ce cas `Country`). Dapper recherchera automatiquement une colonne appelée *Id* sur laquelle se diviser, mais s'il n'en trouve pas et que `splitOn` n'est pas fourni, une `System.ArgumentException` sera lancée avec un message utile. Ainsi, bien que ce soit facultatif, vous devrez généralement fournir une valeur `splitOn`.


## Mappage un-à-plusieurs
Regardons un exemple plus complexe qui contient une relation un-à-plusieurs. Notre requête contiendra désormais plusieurs lignes contenant des données en double et nous devrons gérer cela. Nous faisons cela avec une recherche dans une fermeture.

La requête change légèrement, tout comme les exemples de classes.

| identifiant | Nom | Né | ID pays | NomPays | ID de livre | Nom_livre |
|----|-----------------|------|-----------|------- -------------------|--------|--------------------- ---------------------------------|
| 1 | Daniel Dennett | 1942 | 1 | États-Unis d'Amérique | 1 | Remue-méninges |
| 1 | Daniel Dennett | 1942 | 1 | États-Unis d'Amérique | 2 | Salle des coudes |
| 2 | Sam Harris | 1967 | 1 | États-Unis d'Amérique | 3 | Le paysage moral |
| 2 | Sam Harris | 1967 | 1 | États-Unis d'Amérique | 4 | Se réveiller : un guide de la spiritualité sans religion |
| 3 | Richard Dawkins | 1941 | 2 | Royaume-Uni | 5 | La magie de la réalité : comment nous savons ce qui est vraiment vrai |
| 3 | Richard Dawkins | 1941 | 2 | Royaume-Uni | 6 | Un appétit pour l'émerveillement : la fabrication d'un scientifique |


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

Le dictionnaire "remainingHorsemen" sera rempli avec des instances entièrement matérialisées des objets personne. Pour chaque ligne du résultat de la requête, les valeurs mappées des instances des types définis dans les arguments lambda sont transmises et c'est à vous de décider comment gérer cela.
              
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

> Notez que l'argument `splitOn` est une liste délimitée par des virgules des premières colonnes du type suivant.


## Mappages personnalisés
Si les noms des colonnes de requête ne correspondent pas à vos classes, vous pouvez configurer des mappages pour les types. Cet exemple illustre le mappage à l'aide de `System.Data.Linq.Mapping.ColumnAttribute`ainsi qu'un mappage personnalisé.

> Les mappages n'ont besoin d'être configurés qu'une seule fois par type, donc définissez-les au démarrage de l'application ou ailleurs afin qu'ils ne soient initialisés qu'une seule fois.

En supposant à nouveau la même requête que l'exemple One-to-many et les classes refactorisées vers de meilleurs noms comme suit :


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

> Notez que `Book` ne repose pas sur `ColumnAttribute` mais nous aurions besoin de maintenir l'instruction `if`

Placez maintenant ce code de mappage quelque part dans votre application où il ne sera exécuté qu'une seule fois :

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

Ensuite, la requête est exécutée en utilisant l'un des exemples `Query<>` précédents.

Une manière plus simple d'ajouter les mappages est illustrée dans [cette réponse][1].


[1] : http://stackoverflow.com/a/12615036/2613363

## Mappage de plus de 7 types
Parfois, le nombre de types que vous mappez dépasse les 7 fournis par le Func<> qui effectue la construction.

Au lieu d'utiliser `Query<>` avec les entrées d'argument de type générique, nous fournirons les types à mapper sous forme de tableau, suivis de la fonction de mappage. Hormis le réglage manuel initial et la distribution des valeurs, le reste de la fonction ne change pas.

              
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




