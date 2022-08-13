---
title: "Database Queries"
slug: "database-queries"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Working with databases
One of ColdFusion's strengths is how easy it is to work with databases. And of course, query inputs can and should be parameterized.

Tag Implementation

    <cfquery name="myQuery" datasource="myDatasource" result="myResult">
        select firstName, lastName
        from users
        where lastName = <cfqueryparam value="Allaire" cfsqltype="cf_sql_varchar">
    </cfquery>

CFScript Implementation

    // ColdFusion 9+
    var queryService = new query(name="myQuery", datasource="myDatasource");
    queryService.addParam(name="lName", value="Allaire", cfsqltype="cf_sql_varchar");
    var result = queryService.execute(sql="select firstName, lastName from users where lastName = :lName");
    var myQuery = result.getResult();
    var myResult = result.getPrefix();
   
    // ColdFusion 11+
    var queryParams = {lName = {value="Allaire", cfsqltype="cf_sql_varchar"}};
    var queryOptions = {datasource="myDatasource", result="myResult"};
    var myQuery = queryExecute("select firstName, lastName from users", queryParams, queryOptions);

Inserting values is just as easy:

    queryExecute("
        insert into user( firstname, lastname )
        values( :firstname, :lastname )
    ",{
        firstname: { cfsqltype: "cf_sql_varchar", value: "Dwayne" }
        ,lastname: { cfsqltype: "cf_sql_varchar", value: "Camacho" }
    },{
        result: "local.insertResult"
    });

    return local.insertResult.generated_key;

## Basic Example
Database connections are set up using the CF Administrator tool. See Database Connections for how to connect a datasource. 

To execute queries all you need is the `<cfquery>` tag. The `<cfquery>` tag connects to and opens the database for you, all you need to do is supply it with the name of the datasource.

    <cfquery name="Movies" datasource="Entertainment">
        SELECT title
        FROM   Movies
    </cfquery>

To display the query results:

    <cfoutput query="Movies">
        #title#<BR>
    </cfoutput>



## Authentication
Many database configurations require authentication (in the form of a username and password) before you can query the database. You can supply these using the username and password attributes.

Note: the username and password can also be configured against the datasource in the ColdFusion Administrator. Supplying these details in your query overrides the username and password in the ColdFusion Administrator.

    <cfquery datasource="Entertainment" username="webuser" password="letmein">
        select *
        from Movies
    </cfquery>

## Cached Queries
A cached query is a query that has its results stored in the server's memory. The results are stored when the query is first run. From then on, whenever that query is requested again, ColdFusion will retrieve the results from memory.

You can cache a query using the `cachedAfter` attribute. If the query was last run after the supplied date, cached data is used. Otherwise the query is re-run.

    <cfquery datasource="Entertainment" cachedAfter="July 20, 2016">
        select *
        from Movies
    </cfquery>

In order for the cache to be used, and multiple calls to the database be avoided the current query must use the same SQL statement, data source, query name, user name, and password as the cached query used.  This includes whitespace in the query.  

As such the following queries create different caches, even though the trimmed characters are the same and the query results are identical:

    <cfquery datasource="Entertainment" cachedAfter="July 20, 2016">
        select *
        from Movies
        <cfif false>
        where 1 = 1
        </cfif>
        <cfif true>
        where 1 = 1
        </cfif>
    </cfquery>

    <cfquery datasource="Entertainment" cachedAfter="July 20, 2016">
        select * 
        from Movies
        <cfif true>
        where 1 = 1
        </cfif>
        <cfif false>
        where 1 = 1
        </cfif>
    </cfquery>


## Limiting the Number of Records Returned
You can limit the number of rows to be returned by using the `maxrows` attribute.

    <cfquery datasource="Entertainment" maxrows="50">
        select *
        from Movies
    </cfquery>

## Timeouts
You can set a timeout limit using the `timeout` attribute. This can be useful in preventing requests running far longer than they should and impacting on the whole application as a result.

The `timeout` attribute sets the maximum number of seconds that each action of a query is allowed to execute before returning an error.

    <cfquery datasource="Entertainment" timeout="30">
        select *
        from Movies
    </cfquery>

