---
title: "Multimapeamento"
slug: "multimapeamento"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Sintaxe
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TReturn>(
            this IDbConnection cnn, string sql, Func<TFirst, TSecond, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)` 
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TThird, TFourth, TFifth, TSixth, TSeventh, TReturn>(este IDbConnection cnn, string sql, Func<TFirst, TSecond, TThird, TFourth, TFifth, TSixth, TSeventh, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)`
- `public static IEnumerable<TReturn> Query<TReturn>(este IDbConnection cnn, string sql, Type[] tipos, Func<object[], TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)
        `

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| cnn | Sua conexão com o banco de dados, que já deve estar aberta. |
| SQL | Comando a ser executado.|
| tipos | Matriz de tipos no conjunto de registros. |
| mapa | `Func<>` que trata da construção do resultado de retorno. |
| parâmetro | Objeto do qual extrair parâmetros. |
| transação | Transação da qual esta consulta faz parte, se houver. |
| tamponado | Se deve ou não armazenar em buffer a leitura dos resultados da consulta. Este é um parâmetro opcional com o padrão sendo true. Quando buffer é true, os resultados são armazenados em buffer em um `List<T>` e, em seguida, retornados como um `IEnumerable<T>` que é seguro para enumeração múltipla. Quando o buffer é false, a conexão sql é mantida aberta até que você termine de ler, permitindo que você processe uma única linha de cada vez na memória. Várias enumerações gerarão conexões adicionais com o banco de dados. Embora o buffer false seja altamente eficiente para reduzir o uso de memória, se você mantiver apenas fragmentos muito pequenos dos registros retornados, ele terá uma [sobrecarga de desempenho considerável](http://stackoverflow.com/a/30493725/37055) em comparação com a materialização ansiosa do resultado definir. Por fim, se você tiver várias conexões sql sem buffer simultâneas, precisará considerar a inanição do pool de conexões, fazendo com que as solicitações bloqueiem até que as conexões fiquem disponíveis. |
| splitOn | O campo que devemos dividir e ler o segundo objeto (padrão: id). Esta pode ser uma lista delimitada por vírgulas quando mais de 1 tipo está contido em um registro. |
| comandoTimeout | Número de segundos antes do tempo limite de execução do comando. |
| tipodecomando | É um proc armazenado ou um lote? |

## Mapeamento simples de várias tabelas
Digamos que temos uma consulta dos cavaleiros restantes que precisam preencher uma classe Person.

| Nome | Nascido | Residência |
|-----------------|------|------------------------ --|
| Daniel Dennet | 1942 | Estados Unidos da América |
| Sam Harris | 1967 | Estados Unidos da América |
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

Podemos preencher a classe person assim como a propriedade Residence com uma instância de Country usando uma sobrecarga `Query<>` que recebe um `Func<>` que pode ser usado para compor a instância retornada. O `Func<>` pode receber até 7 tipos de entrada com o argumento genérico final sempre sendo o tipo de retorno.

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

> Observe o uso do argumento `splitOn: "Residence"` que é a 1ª coluna do próximo tipo de classe a ser preenchida (neste caso `Country`). Dapper irá procurar automaticamente por uma coluna chamada *Id* para dividir, mas se não encontrar uma e `splitOn` não for fornecido, um `System.ArgumentException` será lançado com uma mensagem útil. Portanto, embora seja opcional, você normalmente terá que fornecer um valor `splitOn`.


## Mapeamento de um para muitos
Vejamos um exemplo mais complexo que contém um relacionamento um-para-muitos. Nossa consulta agora conterá várias linhas contendo dados duplicados e precisaremos lidar com isso. Fazemos isso com uma pesquisa em um encerramento.

A consulta muda um pouco, assim como as classes de exemplo.

| ID | Nome | Nascido | PaísId | PaísName | BookId | Nome do livro |
|----|-----------------|------|-----------|------- -------------------|--------|--------------------- ----------------------------------|
| 1 | Daniel Dennet | 1942 | 1 | Estados Unidos da América | 1 | Tempestades de Ideias |
| 1 | Daniel Dennet | 1942 | 1 | Estados Unidos da América | 2 | Quarto Cotovelo |
| 2 | Sam Harris | 1967 | 1 | Estados Unidos da América | 3 | A paisagem moral |
| 2 | Sam Harris | 1967 | 1 | Estados Unidos da América | 4 | Acordar: Um Guia para Espiritualidade Sem Religião |
| 3 | Richard Dawkins | 1941 | 2 | Reino Unido | 5 | A magia da realidade: como sabemos o que é realmente verdade |
| 3 | Richard Dawkins | 1941 | 2 | Reino Unido | 6 | Um Apetite por Maravilha: A Criação de um Cientista |


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

O dicionário`remainingHorsemen` será preenchido com instâncias totalmente materializadas dos objetos pessoa. Para cada linha do resultado da consulta são passados ​​os valores mapeados de instâncias dos tipos definidos nos argumentos lambda e cabe a você como lidar com isso.
              
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

> Observe como o argumento `splitOn` é uma lista delimitada por vírgulas das primeiras colunas do próximo tipo.


## Mapeamentos personalizados
Se os nomes das colunas de consulta não corresponderem às suas classes, você poderá configurar mapeamentos para tipos. Este exemplo demonstra o mapeamento usando `System.Data.Linq.Mapping.ColumnAttribute`, bem como um mapeamento personalizado.

> Os mapeamentos só precisam ser configurados uma vez por tipo, portanto, defina-os na inicialização do aplicativo ou em outro lugar para que sejam inicializados apenas uma vez.

Assumindo a mesma consulta do exemplo Um para muitos novamente e as classes refatoradas para nomes melhores, como:


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

>Observe como `Book` não depende de `ColumnAttribute` mas precisaríamos manter a instrução `if`

Agora coloque este código de mapeamento em algum lugar em seu aplicativo onde ele é executado apenas uma vez:

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

Em seguida, a consulta é executada usando qualquer um dos exemplos anteriores de `Query<>`.

Uma maneira mais simples de adicionar os mapeamentos é mostrada em [esta resposta][1].


[1]: http://stackoverflow.com/a/12615036/2613363

## Mapeando mais de 7 tipos
Às vezes, o número de tipos que você está mapeando excede os 7 fornecidos pelo Func<> que faz a construção.

Em vez de usar o `Query<>` com as entradas de argumento de tipo genérico, forneceremos os tipos para mapear como um array, seguido pela função de mapeamento. Além da configuração manual inicial e da conversão dos valores, o restante da função não muda.

              
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




