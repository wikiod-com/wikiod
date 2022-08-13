---
title: "Basic Querying"
slug: "basic-querying"
draft: false
images: []
weight: 9748
type: docs
toc: true
---

## Syntax
 - public static IEnumerable&lt;T&gt; Query&lt;T&gt;(this IDbConnection cnn, string sql, object param = null, SqlTransaction transaction = null, bool buffered = true)
 - public static IEnumerable&lt;dynamic&gt; Query (this IDbConnection cnn, string sql, object param = null, SqlTransaction transaction = null, bool buffered = true)

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| cnn       | Your database connection, which must already be open. |  
| sql       | Command to execute. |  
| param     | Object to extract parameters from. |
| transaction | Transaction which this query is a part of, if any.  |
| buffered    | Whether or not to buffer reading the results of the query. This is an optional parameter with the default being true. When buffered is true, the results are buffered into a `List<T>` and then returned as an `IEnumerable<T>` that is safe for multiple enumeration. When buffered is false, the sql connection is held open until you finish reading allowing you to process a single row at time in memory. Multiple enumerations will spawn additional connections to the database. While buffered false is highly efficient for reducing memory usage if you only maintain very small fragments of the records returned it has a [sizeable performance overhead](http://stackoverflow.com/a/30493725/37055) compared to eagerly materializing the result set. Lastly if you have numerous concurrent unbuffered sql connections you need to consider connection pool starvation causing requests to block until connections become available.   |


## Querying for a static type
For types known at compile-time, use a generic parameter with `Query<T>`.

    public class Dog
    {
        public int? Age { get; set; }
        public Guid Id { get; set; }
        public string Name { get; set; }
        public float? Weight { get; set; }
    
        public int IgnoredProperty { get { return 1; } }
    }    
    
    //
    IDBConnection db = /* ... */;

    var @params = new { age = 3 };
    var sql = "SELECT * FROM dbo.Dogs WHERE Age = @age";

    IEnumerable<Dog> dogs = db.Query<Dog>(sql, @params);

## Querying for dynamic types
You can also query dynamically if you leave off the generic type.
    
    IDBConnection db = /* ... */;
    IEnumerable<dynamic> result = db.Query("SELECT 1 as A, 2 as B");

    var first = result.First();
    int a = (int)first.A; // 1
    int b = (int)first.B; // 2

## Query with Dynamic Parameters
    var color = "Black";
    var age = 4;

    var query = "Select * from Cats where Color = :Color and Age > :Age";
    var dynamicParameters = new DynamicParameters();
    dynamicParameters.Add("Color", color);
    dynamicParameters.Add("Age", age);

    using (var connection = new SqlConnection(/* Your Connection String Here */))
    {
        IEnumerable<dynamic> results = connection.Query(query, dynamicParameters);
    }

