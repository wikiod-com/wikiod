---
title: "Multiple Results"
slug: "multiple-results"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Syntax
 - public static SqlMapper.GridReader QueryMultiple(this IDbConnection cnn, string sql, object param = null, IDbTransaction transaction = null, int? commandTimeout = null, CommandType? commandType = null)
 - public static SqlMapper.GridReader QueryMultiple(this IDbConnection cnn, CommandDefinition command)

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| cnn | Your database connection, must already be open |  
| sql | The sql string to process, contains multiple queries |
| param | Object to extract parameters from |
| SqlMapper.GridReader | Provides interfaces for reading multiple result sets from a Dapper query |


## Base Multiple Results Example
To fetch multiple grids in a single query, the `QueryMultiple` method is used. This then allows you to retrieve each grid *sequentially* through successive calls against the `GridReader` returned.

    var sql = @"select * from Customers where CustomerId = @id
                select * from Orders where CustomerId = @id
                select * from Returns where CustomerId = @id";
    
    using (var multi = connection.QueryMultiple(sql, new {id=selectedId}))
    {
       var customer = multi.Read<Customer>().Single();
       var orders = multi.Read<Order>().ToList();
       var returns = multi.Read<Return>().ToList();
    } 

