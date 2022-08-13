---
title: "Multimapping"
slug: "multimapping"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Syntax
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TReturn>(
            this IDbConnection cnn, string sql, Func<TFirst, TSecond, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)` 
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TThird, TFourth, TFifth, TSixth, TSeventh, TReturn>(this IDbConnection cnn, string sql, Func<TFirst, TSecond, TThird, TFourth, TFifth, TSixth, TSeventh, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)`
 - `public static IEnumerable<TReturn> Query<TReturn>(this IDbConnection cnn, string sql, Type[] types, Func<object[], TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)
        `

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| cnn     | Your database connection, which must already be open. |  
| sql     | Command to execute.|
| types   | Array of types in the record set. |
| map     | `Func<>` that handles construction of the return result. |
| param   | Object to extract parameters from. |
| transaction | Transaction which this query is a part of, if any. |
| buffered    | Whether or not to buffer reading the results of the query. This is an optional parameter with the default being true. When buffered is true, the results are buffered into a `List<T>` and then returned as an `IEnumerable<T>` that is safe for multiple enumeration. When buffered is false, the sql connection is held open until you finish reading allowing you to process a single row at time in memory. Multiple enumerations will spawn additional connections to the database. While buffered false is highly efficient for reducing memory usage if you only maintain very small fragments of the records returned it has a [sizeable performance overhead](http://stackoverflow.com/a/30493725/37055) compared to eagerly materializing the result set. Lastly if you have numerous concurrent unbuffered sql connections you need to consider connection pool starvation causing requests to block until connections become available.   |
| splitOn    | The Field we should split and read the second object from (default: id). This can be a comma delimited list when more than 1 type is contained in a record. |
| commandTimeout | Number of seconds before command execution timeout. |
| commandType | Is it a stored proc or a batch? |

## Simple multi-table mapping
Let's say we have a query of the remaining horsemen that needs to populate a Person class.

| Name            | Born | Residence                |
|-----------------|------|--------------------------|
| Daniel Dennett  | 1942 | United States of America |
| Sam Harris      | 1967 | United States of America |
| Richard Dawkins | 1941 | United Kingdom           |

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

We can populate the person class as well as the Residence property with an instance of Country using an overload `Query<>` that takes a `Func<>` that can be used to compose the returned instance. The `Func<>` can take up to 7 input types with the final generic argument always being the return type.

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

> Note the use of the `splitOn: "Residence"` argument which is the 1st column of the next class type to be populated (in this case `Country`). Dapper will automatically look for a column called *Id* to split on but if it does not find one and `splitOn` is not provided a `System.ArgumentException` will be thrown with a helpful message. So although it is optional you will usually have to supply a `splitOn` value.


## One-to-many mapping
Let's look at a more complex example that contains a one-to-many relationship. Our query will now contain multiple rows containing duplicate data and we will need to handle this. We do this with a lookup in a closure.

The query changes slightly as do the example classes.

| Id | Name            | Born | CountryId | CountryName              | BookId | BookName                                             |
|----|-----------------|------|-----------|--------------------------|--------|------------------------------------------------------|
| 1  | Daniel Dennett  | 1942 | 1         | United States of America | 1      | Brainstorms                                          |
| 1  | Daniel Dennett  | 1942 | 1         | United States of America | 2      | Elbow Room                                           |
| 2  | Sam Harris      | 1967 | 1         | United States of America | 3      | The Moral Landscape                                  |
| 2  | Sam Harris      | 1967 | 1         | United States of America | 4      | Waking Up: A Guide to Spirituality Without Religion  |
| 3  | Richard Dawkins | 1941 | 2         | United Kingdom           | 5      | The Magic of Reality: How We Know What`s Really True |
| 3  | Richard Dawkins | 1941 | 2         | United Kingdom           | 6      | An Appetite for Wonder: The Making of a Scientist    |


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

The dictionary`remainingHorsemen` will be populated with fully materialized instances of the person objects. For each row of the query result the mapped values of instances of the types defined in the lambda arguments are passed in and it is up to you how to handle this.
              
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

> Note how the `splitOn` argument is a comma delimited list of the first columns of the next type.


## Custom Mappings
If the query column names do not match your classes you can setup mappings for types. This example demonstrates mapping using `System.Data.Linq.Mapping.ColumnAttribute`as well as a custom mapping.

> The mappings only need to be setup once per type so set them on application startup or somewhere else that they are only initialized once.

Assuming the same query as the One-to-many example again and the classes refactored toward better names like so:


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

>Note how `Book` doesn't rely on `ColumnAttribute` but we would need to maintain the `if` statement

Now place this mapping code somewhere in your application where it is only executed once:

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

Then the query is executed using any of the previous `Query<>` examples.

A simpler way of adding the mappings is shown in [this answer][1].


  [1]: http://stackoverflow.com/a/12615036/2613363

## Mapping more than 7 types
Sometimes the number of types you are mapping exceeds the 7 provided by the Func<> that does the construction.

Instead of using the `Query<>` with the generic type argument inputs, we will provide the types to map to as an array, followed by the mapping function. Other than the initial manual setting and casting of the values, the rest of the function does not change.

              
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




