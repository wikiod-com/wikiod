---
title: "Dynamic Parameters"
slug: "dynamic-parameters"
draft: false
images: []
weight: 9497
type: docs
toc: true
---

## Basic Usage
It isn't always possible to neatly package all the parameters up in a single object / call. To help with more complicated scenarios, dapper allows the `param` parameter to be an `IDynamicParameters` instance. If you do this, your custom `AddParameters` method is called at the appropriate time and handed the command to append to. In most cases, however, it is sufficient to use the pre-existing `DynamicParameters` type:

    var p = new DynamicParameters(new { a = 1, b = 2 });
    p.Add("c", dbType: DbType.Int32, direction: ParameterDirection.Output);
    connection.Execute(@"set @c = @a + @b", p);
    int updatedValue = p.Get<int>("@c");

This shows:

- (optional) population from an existing object
- (optional) adding additional parameters on the fly
- passing the parameters to the command
- retrieving any updated value after the command has finished

Note that due to how RDBMS protocols work, it is usually only reliable to obtain updated parameter values **after** any data (from a `Query` or QueryMultiple` operation) has been **fully** consumed (for example, on SQL Server, updated parameter values are at the *end* of the TDS stream).

## Dynamic Parameters in Dapper
    
    connection.Execute(@"some Query with @a,@b,@c", new {a=somevalueOfa,b=somevalueOfb,c=somevalueOfc});

## Using a template object
You can use an instance of an object to form your parameters

    public class SearchParameters {
      public string SearchString { get; set; }
      public int Page { get; set; }
    }

    var template= new SearchParameters {
      SearchString = "Dapper",
      Page = 1
    };

    var p = new DynamicParameters(template);

You can also use an anonymous object or a `Dictionary`

