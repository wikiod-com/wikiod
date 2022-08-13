---
title: "Getting started with linq"
slug: "getting-started-with-linq"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
LINQ requires .NET 3.5 or higher (or .NET 2.0 using [LINQBridge](https://bitbucket.org/raboof/linqbridge)).

Add a reference to **System.Core**, if it hasn't been added yet.

At the top of the file, import the namespace:

* C#

<!-- language: lang-c# -->

      using System;
      using System.Linq;

* VB.NET

<!-- language: lang-vb -->
    
      Imports System.Linq

## The different joins in LINQ
In the following examples, we'll be  using the following samples:

    List<Product> Products = new List<Product>()
    {
      new Product()
      {
        ProductId = 1,
        Name = "Book nr 1",
        Price = 25
      },
      new Product()
      {
        ProductId = 2,
        Name = "Book nr 2",
        Price = 15
      },
      new Product()
      {
        ProductId = 3,
        Name = "Book nr 3",
        Price = 20
      },
    };
    List<Order> Orders = new List<Order>()
    {
      new Order()
      {
        OrderId = 1,
        ProductId = 1,
      },
      new Order()
      {
        OrderId = 2,
        ProductId = 1,
      },
      new Order()
      {
        OrderId = 3,
        ProductId = 2,
      },
      new Order()
      {
        OrderId = 4,
        ProductId = NULL,
      },
    };

**INNER JOIN**
<br><br>
**Query Syntax**

    var joined = (from p in Products
                  join o in Orders on p.ProductId equals o.ProductId
                  select new
                  {
                    o.OrderId,
                    p.ProductId,
                    p.Name
                  }).ToList();

**Method Syntax**
<br>

    var joined = Products.Join(Orders, p => p.ProductId, 
                                       o => o.OrderId, 
                                         => new 
                                        { 
                                          OrderId   = o.OrderId, 
                                          ProductId = p.ProductId, 
                                          Name      = p.Name 
                                        })
                         .ToList();

Result:

    { 1, 1, "Book nr 1" },
    { 2, 1, "Book nr 1" },
    { 3, 2, "Book nr 2" }

**LEFT OUTER JOIN**

    var joined = (from p in Products
                  join o in Orders on p.ProductId equals o.ProductId into g
                  from lj in g.DefaultIfEmpty()
                  select new
                  {
                    //For the empty records in lj, OrderId would be NULL
                    OrderId = (int?)lj.OrderId,
                    p.ProductId,
                    p.Name
                  }).ToList();

Result:

    { 1, 1, "Book nr 1" },
    { 2, 1, "Book nr 1" },
    { 3, 2, "Book nr 2" },
    { NULL, 3, "Book nr 3" }

**CROSS JOIN**

    var joined = (from p in Products
                  from o in Orders
                  select new
                  {
                    o.OrderId,
                    p.ProductId,
                    p.Name
                  }).ToList();

Result:

    { 1, 1, "Book nr 1" },
    { 2, 1, "Book nr 1" },
    { 3, 2, "Book nr 2" },
    { NULL, 3, "Book nr 3" },
    { 4, NULL, NULL }

**GROUP JOIN**

    var joined = (from p in Products
                  join o in Orders on p.ProductId equals o.ProductId
                    into t
                  select new
                  {
                    p.ProductId,
                    p.Name,
                    Orders = t
                  }).ToList();

The Propertie `Orders` now contains an `IEnumerable<Order>` with all linked Orders.

Result:

    { 1, "Book nr 1", Orders = { 1, 2 } },
    { 2, "Book nr 2", Orders = { 3 } },
    { 3, "Book nr 3", Orders = { } },

**How to join on multiple conditions**

When joining on a single condition, you can use:

    join o in Orders 
      on p.ProductId equals o.ProductId

When joining on multiple, use:

    join o in Orders 
      on new { p.ProductId, p.CategoryId } equals new { o.ProductId, o.CategoryId }

Make sure that both anonymous objects have the same properties, and in VB.NET, they must be marked `Key`, although VB.NET allows multiple `Equals` clauses separated by `And`:

<!-- language: lang-vb -->

    Join o In Orders 
      On p.ProductId Equals o.ProductId And p.CategoryId Equals o.CategoryId



## Query Syntax and Method Syntax
Query syntax and method syntax are semantically identical, but many people find query syntax simpler and easier to read. Let’s say we need to retrieve all even items ordered in ascending order from a collection of numbers.

C#:

<!-- language: lang-c# -->

    int[] numbers = { 0, 1, 2, 3, 4, 5, 6 };

    // Query syntax:
    IEnumerable<int> numQuery1 =
                from num in numbers
                where num % 2 == 0
                orderby num
                select num;

    // Method syntax:
    IEnumerable<int> numQuery2 = numbers.Where(num => num % 2 == 0).OrderBy(n => n);

VB.NET:

<!-- language: lang-vb -->

    Dim numbers() As Integer = { 0, 1, 2, 3, 4, 5, 6 }

    ' Query syntax: '
    Dim numQuery1 = From num In numbers
                     Where num Mod 2 = 0
                     Select num
                     Order By num

    ' Method syntax: '
    Dim numQuery2 = numbers.where(Function(num) num Mod 2 = 0).OrderBy(Function(num) num)

Remember that some queries **must** be expressed as method calls. For example, you must use a method call to express a query that retrieves the number of elements that match a specified condition. You also must use a method call for a query that retrieves the element that has the maximum value in a source sequence. So that might be an advantage of using method syntax to make the code more consistent. However, of course you can always apply the method after a query syntax call:

C#:

<!-- language: lang-c# -->

    int maxNum =
        (from num in numbers
         where num % 2 == 0
         select num).Max();

VB.NET:

<!-- language: lang-vb -->

    Dim maxNum =
        (From num In numbers
         Where num Mod 2 = 0
         Select num).Max();


## LINQ methods, and IEnumerable<T> vs IQueryable<T>
LINQ extension methods on `IEnumerable<T>` take actual methods<sup>1</sup>, whether anonymous methods:

    //C#
    Func<int,bool> fn = x => x > 3;
    var list = new List<int>() {1,2,3,4,5,6};
    var query = list.Where(fn);

    'VB.NET
    Dim fn = Function(x As Integer) x > 3
    Dim list = New List From {1,2,3,4,5,6};
    Dim query = list.Where(fn);

or named methods (methods explicitly defined as part of a class):

    //C#
    class Program {
        bool LessThan4(int x) {
            return x < 4;
        }

        void Main() {
            var list = new List<int>() {1,2,3,4,5,6};
            var query = list.Where(LessThan4);
        }
    }

    'VB.NET
    Class Program
        Function LessThan4(x As Integer) As Boolean
            Return x < 4
        End Function
        Sub Main
            Dim list = New List From {1,2,3,4,5,6};
            Dim query = list.Where(AddressOf LessThan4)
        End Sub
    End Class

In theory, it is possible to [parse the method's IL](http://stackoverflow.com/q/5667816), figure out what the method is trying to do, and apply that method's logic to any underlying data source, not just objects in memory. But parsing IL is not for the faint of heart.

---

Fortunately, .NET provides the `IQueryable<T>` interface, and the extension methods at `System.Linq.Queryable`, for this scenario. These extension methods take an expression tree &mdash; a data structure representing code &mdash; instead of an actual method, which the LINQ provider can then parse<sup>2</sup> and convert to a more appropriate form for querying the underlying data source. For example:

    //C#
    IQueryable<Person> qry = PersonsSet();

    // Since we're using a variable of type Expression<Func<Person,bool>>, the compiler 
    // generates an expression tree representing this code
    Expression<Func<Person,bool>> expr = x => x.LastName.StartsWith("A");
    // The same thing happens when we write the lambda expression directly in the call to 
    // Queryable.Where
    
    qry = qry.Where(expr);


    'VB.NET
    Dim qry As IQueryable(Of Person) = PersonSet()
    
    ' Since we're using a variable of type Expression(Of Func(Of Person,Boolean)), the compiler 
    ' generates an expression tree representing this code
    Dim expr As Expression(Of Func(Of Person, Boolean)) = Function(x) x.LastName.StartsWith("A")
    ' The same thing happens when we write the lambda expression directly in the call to 
    ' Queryable.Where

    qry = qry.Where(expr)

If (for example) this query is against a SQL database, the provider could convert this expression to the following SQL statement:

    SELECT *
    FROM Persons
    WHERE LastName LIKE N'A%'

and execute it against the data source.

On the other hand, if the query is against a REST API, the provider could convert the same expression to an API call:

    http://www.example.com/person?filtervalue=A&filtertype=startswith&fieldname=lastname

There are two primary benefits in tailoring a data request based on an expression (as opposed to loading the entire collection into memory and querying locally):

* The underlying data source can often query more efficiently. For example, there may very well be an index on `LastName`. Loading the objects into local memory and querying in-memory loses that efficiency.
* The data can be shaped and reduced before it is transferred. In this case, the database / web service only needs to return the matching data, as opposed to the entire set of Persons available from the data source.

---
**Notes**  
<sub>1. Technically, they don't actually take methods, but rather [delegate instances which point to methods](http://programmers.stackexchange.com/a/314086/100120). However, this distinction is irrelevant here.</sub>  
<sub>2. This is the reason for [errors](http://stackoverflow.com/questions/5899683/linq-to-entities-does-not-recognize-the-method-system-string-tostring-method) [like](http://stackoverflow.com/q/10110266) "_LINQ to Entities does not recognize the method 'System.String ToString()' method, and this method cannot be translated into a store expression._". The LINQ provider (in this case the Entity Framework provider) doesn't know how to parse and translate a call to `ToString` to equivalent SQL.</sub>


