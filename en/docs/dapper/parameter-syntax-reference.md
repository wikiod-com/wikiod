---
title: "Parameter Syntax Reference"
slug: "parameter-syntax-reference"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| `this cnn` | The underlying database connection - the `this` denotes an extension method; the connection does not need to be open - if it is not open, it is opened and closed automatically.
| `<T>` / `Type` | (optional) The type of object to return; if the non-generic / non-`Type` API is used, a `dynamic` object is returned per row, simulating a property named per column name  returned from the query (this `dynamic` object also implements `IDicionary<string,object>`).
| `sql` | The SQL to execute
| `param` | (optional) The parameters to include.
| `transaction` | (optional) The database transaction to associate with the command
| `buffered` | (optional) Whether to pre-consume the data into a list (the default), versus exposing an open `IEnumerable` over the live reader
| `commandTimeout` | (optional) The timeout to use on the command; if not specified, `SqlMapper.Settings.CommandTimeout` is assumed (if specified)
| `commandType` | The type of command being performed; defaults to `CommandText`

The syntax for expressing parameters varies between RDBMS. All the examples above use SQL Server syntax, i.e. `@foo`; however, `?foo` and `:foo` should also work fine.

## Value Inlining
Sometimes the convenience of a parameter (in terms of maintenance and expressiveness), may be outweighed by its cost in performance to treat it as a parameter. For example, when page size is fixed by a configuration setting. Or a status value is matched to an `enum` value. Consider:

    var orders = connection.Query<Order>(@"
    select top (@count) * -- these brackets are an oddity of SQL Server
    from Orders
    where CustomerId = @customerId
    and Status = @open", new { customerId, count = PageSize, open = OrderStatus.Open });

The only *real* parameter here is `customerId` - the other two are pseudo-parameters that won't actually change. Often the RDBMS can do a better job if it detects these as constants. Dapper has a special syntax for this - `{=name}` instead of `@name` - which *only* applies to numeric types. (This minimizes any attack surface from SQL injection). An example is as follows:

    var orders = connection.Query<Order>(@"
    select top {=count} *
    from Orders
    where CustomerId = @customerId
    and Status = {=open}", new { customerId, count = PageSize, open = OrderStatus.Open });

Dapper replaces values with literals before issuing the SQL, so the RDBMS actually sees something like:

    select top 10 *
    from Orders
    where CustomerId = @customerId
    and Status = 3

This is particularly useful when allowing RDBMS systems to not just make better decisions, but to open up query plans that actual parameters prevent. For example, if a column predicate is against a parameter, then a filtered index with specific values on that columns cannot be used. This is because the *next* query may have a parameter apart from one of those specified values.

With literal values, the query optimizer is able to make use of the filtered indexes since it knows the value cannot change in future queries.

## Basic Parameterized SQL
Dapper makes it easy to follow best practice by way of fully parameterized SQL.

![Bobby Tables](https://imgs.xkcd.com/comics/exploits_of_a_mom.png)

Parameters are important, so dapper makes it easy to get it right. You just express your parameters in the normal way for your RDBMS (usually `@foo`, `?foo` or `:foo`) and give dapper  an object that *has a member called `foo`*. The most common way of doing this is with an anonymous type:

    int id = 123;
    string name = "abc";
    connection.Execute("insert [KeyLookup](Id, Name) values(@id, @name)",
        new { id, name });

And... that's it. Dapper will add the required parameters and everything should work.

Using your Object Model
---

You can also use your existing object model as a parameter:

    KeyLookup lookup = ... // some existing instance
    connection.Execute("insert [KeyLookup](Id, Name) values(@Id, @Name)", lookup);

Dapper uses the command-text to determine which members of the object to add - it won't usually add unnecessary things like `Description`, `IsActive`, `CreationDate` because the command we've issued clearly doesn't involve them - although there are cases when it might do that, for example if your command contains:

    // TODO - removed for now; include the @Description in the insert

It doesn't attempt to figure out that the above is just a comment.

Stored Procedures
---

Parameters to stored procedures work exactly the same, except that dapper cannot attempt to determine what should/should-not be included - everything available is treated as a parameter. For that reason, anonymous types are usually preferred:

    connection.Execute("KeyLookupInsert", new { id, name },
        commandType: CommandType.StoredProcedure);



## List Expansions
A common scenario in database queries is `IN (...)` where the list here is generated at runtime. Most RDBMS lack a good metaphor for this - and there is no universal *cross-RDBMS* solution for this. Instead, dapper provides some gentle automatic command expansion. All that is requires is a supplied parameter value that is `IEnumerable`. A command involving `@foo` is expanded to `(@foo0,@foo1,@foo2,@foo3)` (for a sequence of 4 items). The most common usage of this would be `IN`:

    int[] orderIds = ...
    var orders = connection.Query<Order>(@"
    select *
    from Orders
    where Id in @orderIds", new { orderIds });

This then automatically expands to issue appropriate SQL for the multi-row fetch:

    select *
    from Orders
    where Id in (@orderIds0, @orderIds1, @orderIds2, @orderIds3)

with the parameters `@orderIds0` etc being added as values taken from the arrray.
Note that the fact that it isn't valid SQL originally is intentional, to ensure that this feature is not used mistakenly. This feature also works correctly with the `OPTIMIZE FOR` / `UNKNOWN` query-hint in SQL Server; if you use:

    option (optimize for
        (@orderIds unknown))

it will expand  this correctly to:

    option (optimize for
        (@orderIds0 unknown, @orderIds1 unknown, @orderIds2 unknown, @orderIds3 unknown))

## Performing Operations Against Multiple Sets of Input
Sometimes, you want to do the same thing multiple times. Dapper supports this on the `Execute` method if the *outermost* parameter (which is usually a single anonymous type, or a domain model instance) is actually provided as an `IEnumerable` sequence. For example:

    Order[] orders = ...
    // update the totals
    connection.Execute("update Orders set Total=@Total where Id=@Id", orders);

Here, dapper is just doing a simple loop on our data, essentially the same as if we had done:

    Order[] orders = ...
    // update the totals
    foreach(Order order in orders) {
        connection.Execute("update Orders set Total=@Total where Id=@Id", order);
    }

This usage becomes *particularly* interesting when combined with the `async` API on a connection that is explicitly configured to all "Multiple Active Result Sets" - in this usage, dapper will automatically *pipeline* the operations, so you aren't paying the latency cost per row. This requires a slightly more complicated usage,  

    await connection.ExecuteAsync(
        new CommandDefinition(
            "update Orders set Total=@Total where Id=@Id", 
             orders, flags: CommandFlags.Pipelined))

Note, however, that you might also wish to investigate table valued parameters.

## Pseudo-Positional Parameters (for providers that don't support named parameters)
Some ADO.NET providers (most notably: OleDB) do not support *named* parameters; parameters are instead specified only by *position*, with the `?` place-holder. Dapper would not know what member to use for these, so dapper allows an alternative syntax, `?foo?`; this would be the same as `@foo` or `:foo` in other SQL variants, except that dapper will **replace** the parameter token completely with `?` before executing the query.

This works in combination with other features such as list expansion, so the following is valid:

    string region = "North";
    int[] users = ...
    var docs = conn.Query<Document>(@"
         select * from Documents
         where Region = ?region?
         and OwnerId in ?users?", new { region, users }).AsList();

The `.region` and `.users` members are used accordingly, and the SQL issued is (for example, with 3 users):

         select * from Documents
         where Region = ?
         and OwnerId in (?,?,?)

Note, however, that dapper **does not** allow the same parameter to be used multiple times when using this feature; this is to prevent having to add the same parameter value (which could be large) multiple times. If you need to refer to the same value multiple times, consider declaring a variable, for example:

    declare @id int = ?id?; // now we can use @id multiple times in the SQL

If variables are not available, you can use duplicate member names in the parameters - this will also make it obvious that the value is being sent multiple times:

    int id = 42;
    connection.Execute("... where ParentId = $id0$ ... SomethingElse = $id1$ ...",
          new { id0 = id, id1 = id });

