---
title: "Method execution modes - immediate, deferred streaming, deferred non-streaming"
slug: "method-execution-modes---immediate-deferred-streaming-deferred-non-streaming"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Deferred execution vs immediate execution
Some LINQ methods return a query object. This object does not hold the results of the query; instead, it has all the information needed to generate those results:

    var list = new List<int>() {1, 2, 3, 4, 5};
    var query = list.Select(x => {
        Console.Write($"{x} ");
        return x;
    });

The query contains a call to `Console.Write`, but nothing has been output to the console. This is because the query hasn't been executed yet, and thus the function passed to `Select` has never been evaluated. This is known as [**deferred execution**](https://msdn.microsoft.com/en-us/library/mt693095.aspx?f=255&MSPPError=-2147217396) -- the query's execution is delayed until some later point.

Other LINQ methods force an [**immediate execution**](https://msdn.microsoft.com/en-us/library/mt693095.aspx) of the query; these methods execute the query and generate its values:

    var newList = query.ToList();

At this point, the function passed into `Select` will be evaluated for each value in the original list, and the following will be output to the console:

> 1 2 3 4 5

---
Generally, LINQ methods which return a single value (such as `Max` or `Count`), or which return an object that actually holds the values (such as `ToList` or `ToDictionary`) execute immediately. 

Methods which return an `IEnumerable<T>` or `IQueryable<T>` are returning the query object, and allow deferring the execution until a later point.

Whether a particular LINQ method forces a query to execute immediately or not, can be found at MSDN -- [C#](https://msdn.microsoft.com/en-us/library/mt693095.aspx), or [VB.NET](https://msdn.microsoft.com/en-us/library/mt692840.aspx).

## Streaming mode (lazy evaluation) vs non-streaming mode (eager evaluation)
Of the LINQ methods which use deferred execution, some require a single value to be evaluated at a time. The following code:
 
    var lst = new List<int>() {3, 5, 1, 2};
    var streamingQuery = lst.Select(x => {
        Console.WriteLine(x);
        return x;
    });
    foreach (var i in streamingQuery) {
        Console.WriteLine($"foreach iteration value: {i}");
    }
 
will output:
 
> 3  
> foreach iteration value: 3  
> 5  
> foreach iteration value: 5  
> 1  
> foreach iteration value: 1  
> 2  
> foreach iteration value: 2  

because the function passed to `Select` is evaluated at each iteration of the `foreach`. This is known as [**streaming mode**](https://msdn.microsoft.com/en-us/library/mt693095.aspx?f=255&MSPPError=-2147217396) or [**lazy evaluation**](https://msdn.microsoft.com/en-us/library/mt693152.aspx#Anchor_1).
 
---
 
Other LINQ methods -- sorting and grouping operators -- require _all_ the values to be evaluated, before they can return _any_ value:
 
    var nonStreamingQuery = lst.OrderBy(x => {
        Console.WriteLine(x);
        return x;
    });
    foreach (var i in nonStreamingQuery) {
        Console.WriteLine($"foreach iteration value: {i}");
    }
 
will output:
 
> 3  
> 5  
> 1  
> 2    
> foreach iteration value: 1  
> foreach iteration value: 2  
> foreach iteration value: 3  
> foreach iteration value: 5  
 
In this case, because the values must be generated to the `foreach` in ascending order, all the elements must first be evaluated, in order to determine which is the smallest, and which is the next smallest, and so on. This is known as [**non-streaming mode**](https://msdn.microsoft.com/en-us/library/mt693095.aspx?f=255&MSPPError=-2147217396) or [**eager evaluation**](https://msdn.microsoft.com/en-us/library/mt693152.aspx#Anchor_1).
 
---
 
Whether a particular LINQ method uses streaming or non-streaming mode, can be found at MSDN -- [C#](https://msdn.microsoft.com/en-us/library/mt693095.aspx), or [VB.NET](https://msdn.microsoft.com/en-us/library/mt692840.aspx).

## Benefits of deferred execution - building queries
Deferred execution enables combining different operations to build the final query, before evaluating the values:

    var list = new List<int>() {1,1,2,3,5,8};
    var query = list.Select(x => x + 1);

If we execute the query at this point:

    foreach (var x in query) {
        Console.Write($"{x} ");
    }

we would get the following output:

> 2 2 3 4 6 9

But we can modify the query by adding more operators:

    Console.WriteLine();
    query = query.Where(x => x % 2 == 0);
    query = query.Select(x => x * 10);

    foreach (var x in query) {
        Console.Write($"{x} ");
    }

Output:

> 20 20 40 60

## Benefits of deferred execution - querying current data
With deferred execution, if the data to be queried is changed, the query object uses the data at the time of execution, not at the time of definition.

    var data = new List<int>() {2, 4, 6, 8};
    var query = data.Select(x => x * x);

If we execute the query at this point with an immediate method or `foreach`, the query will operate on the list of even numbers.

However, if we change the values in the list:

    data.Clear();
    data.AddRange(new [] {1, 3, 5, 7, 9});

or even if we assign a a new list to `data`:

    data = new List<int>() {1, 3, 5, 7, 9};
        
and then execute the query, the query will operate on the new value of `data`:

    foreach (var x in query) {
        Console.Write($"{x} ");
    }

and will output the following:

> 1 9 25 49 81

