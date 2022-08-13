---
title: "Azure DocumentDB"
slug: "azure-documentdb"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Connect to an account (.NET)
To connect to your DocumentDB database you will need to create a `DocumentClient` with your **Endpoint URI** and the **Service Key** (you can get both from the portal).

First of all, you will need the following using clauses:

    using System;
    using Microsoft.Azure.Documents.Client;

Then you can create the client with:

    var endpointUri = "<your endpoint URI>";
    var primaryKey = "<your key>";
    var client = new DocumentClient(new Uri(endpointUri), primaryKey);

## Create a database (.NET)
Your DocumentDB **database** can be created by using the `CreateDatabaseAsync` method of the `DocumentClient` class. A database is the logical container of JSON document storage partitioned across collections.

    using System.Net;
    using System.Threading.Tasks;
    using Microsoft.Azure.Documents;
    using Microsoft.Azure.Documents.Client;

To create your database:

    async Task CreateDatabase(DocumentClient client)
    {
        var databaseName = "<your database name>";
        await client.CreateDatabaseAsync(new Database { Id = databaseName });
    }

You can also check if the database already exists and create it if needed:

    async Task CreateDatabaseIfNotExists(DocumentClient client)
    {
        var databaseName = "<your database name>";
        try
        {
            await client.ReadDatabaseAsync(UriFactory.CreateDatabaseUri(databaseName));
        }
        catch (DocumentClientException e)
        {
            // If the database does not exist, create a new database
            if (e.StatusCode == HttpStatusCode.NotFound)
            {
                await client.CreateDatabaseAsync(new Database { Id = databaseName });                        
            }
            else
            {
                // Rethrow
                throw;
            }
        }
    }

## Create a collection (.NET)
A **collection** can be created by using the `CreateDocumentCollectionAsync` method of the `DocumentClient` class. A collection is a container of JSON documents and associated JavaScript application logic.

    async Task CreateCollection(DocumentClient client)
    {
        var databaseName = "<your database name>";
        var collectionName = "<your collection name>";

        DocumentCollection collectionInfo = new DocumentCollection();
        collectionInfo.Id = collectionName;

        // Configure collections for maximum query flexibility including string range queries.
        collectionInfo.IndexingPolicy = new IndexingPolicy(new RangeIndex(DataType.String) { Precision = -1 });

        // Here we create a collection with 400 RU/s.
        await client.CreateDocumentCollectionAsync(UriFactory.CreateDatabaseUri(databaseName),
            collectionInfo, new RequestOptions { OfferThroughput = 400 });
    }

Or you can check if the collection exists and create it if necessary:

    async Task CreateDocumentCollectionIfNotExists(DocumentClient client)
    {
        var databaseName = "<your database name>";
        var collectionName = "<your collection name>";
        try
        {
            await client.ReadDocumentCollectionAsync(UriFactory.CreateDocumentCollectionUri(databaseName, collectionName));
        }
        catch (DocumentClientException e)
        {
            // If the document collection does not exist, create a new collection
            if (e.StatusCode == HttpStatusCode.NotFound)
            {
                DocumentCollection collectionInfo = new DocumentCollection();
                collectionInfo.Id = collectionName;

                // Configure collections for maximum query flexibility including string range queries.
                collectionInfo.IndexingPolicy = new IndexingPolicy(new RangeIndex(DataType.String) { Precision = -1 });

                // Here we create a collection with 400 RU/s.
                await client.CreateDocumentCollectionAsync(UriFactory.CreateDatabaseUri(databaseName),
                    collectionInfo, new RequestOptions { OfferThroughput = 400 });
            }
            else
            {
                // Rethrow
                throw;
            }
        }
    }

## Create JSON documents (.NET)
A **document** can be created by using the `CreateDocumentAsync` method of the `DocumentClient` class. Documents are user defined (arbitrary) JSON content.

    async Task CreateFamilyDocumentIfNotExists(DocumentClient client, string databaseName, string collectionName, Family family)
    {
        try
        {
            await client.ReadDocumentAsync(UriFactory.CreateDocumentUri(databaseName, collectionName, family.Id));
        }
        catch (DocumentClientException e)
        {
            if (e.StatusCode == HttpStatusCode.NotFound)
            {
                await client.CreateDocumentAsync(UriFactory.CreateDocumentCollectionUri(databaseName, collectionName), family);
            }
            else
            {
                // Rethrow
                throw;
            }
        }
    }

Having the following classes that represent a (simplified) family:

    public class Family
    {
        [JsonProperty(PropertyName = "id")]
        public string Id { get; set; }
        public string LastName { get; set; }
        public Parent[] Parents { get; set; }
        public Child[] Children { get; set; }
        public Address Address { get; set; }
        public bool IsRegistered { get; set; }
        public override string ToString()
        {
            return JsonConvert.SerializeObject(this);
        }
    }

    public class Parent
    {
        public string FamilyName { get; set; }
        public string FirstName { get; set; }
    }

    public class Child
    {
        public string FamilyName { get; set; }
        public string FirstName { get; set; }
        public string Gender { get; set; }
        public int Grade { get; set; }
        public Pet[] Pets { get; set; }
    }

    public class Pet
    {
        public string GivenName { get; set; }
    }

    public class Address
    {
        public string State { get; set; }
        public string County { get; set; }
        public string City { get; set; }
    }



## Query for documents (.NET)
DocumentDB supports rich **queries** against **JSON documents** stored in each collection.

With a LINQ query
=================

    IQueryable<Family> familyQuery = this.client.CreateDocumentQuery<Family>(
        UriFactory.CreateDocumentCollectionUri(databaseName, collectionName), queryOptions)
        .Where(f => f.LastName == "Andersen");

With a SQL query
================

    IQueryable<Family> familyQueryInSql = this.client.CreateDocumentQuery<Family>(
        UriFactory.CreateDocumentCollectionUri(databaseName, collectionName),
        "SELECT * FROM Family WHERE Family.lastName = 'Andersen'",
        queryOptions);

Pagination on a LINQ query
================

The [FeedOptions](https://msdn.microsoft.com/en-us/library/microsoft.azure.documents.client.feedoptions.aspx) is used to set the [RequestContinuation](https://msdn.microsoft.com/en-us/library/microsoft.azure.documents.client.feedoptions.requestcontinuation.aspx) property obtained on the first query:

    public async Task<IEnumerable<Family>> QueryWithPagination(int Size_of_Page)
    {
        var queryOptions = new FeedOptions() { MaxItemCount = Size_of_Page };
        string continuationToken = string.Empty;
        do
        {
            if (!string.IsNullOrEmpty(continuationToken))
            {
                queryOptions.RequestContinuation = continuationToken;
            }

            IDocumentQuery<Family> familyQuery = this.client.CreateDocumentQuery<Family>(
                UriFactory.CreateDocumentCollectionUri(databaseName, collectionName), queryOptions)
                .Where(f => f.LastName == "Andersen").AsDocumentQuery();

            var queryResult = await familyQuery.ExecuteNextAsync<Family>();
            continuationToken = queryResult.ResponseContinuation;
            yield return queryResult;

        } while (!string.IsNullOrEmpty(continuationToken));
    }
 
You can always call once and return the Continuation Token to the client, so the paginated request is sent when the client wants the next page. Using a helper class and an extension:

    public class PagedResults<T>
    {
        public PagedResults()
        {
            Results = new List<T>();
        }
        public string ContinuationToken { get; set; }
        public List<T> Results { get; set; }
    }

    public async Task<PagedResults<Family>> QueryWithPagination(int Size_of_Page, string continuationToken = "")
    {
        var queryOptions = new FeedOptions() { MaxItemCount = Size_of_Page };
        if (!string.IsNullOrEmpty(continuationToken))
        {
            queryOptions.RequestContinuation = continuationToken;
        }
        
        return await familyQuery = this.client.CreateDocumentQuery<Family>(
            UriFactory.CreateDocumentCollectionUri(databaseName, collectionName), queryOptions)
            .Where(f => f.LastName == "Andersen").ToPagedResults();
    }
    
    
    public static class DocumentDBExtensions
    {
        public static async Task<PagedResults<T>> ToPagedResults<T>(this IQueryable<T> source)
            {
                var documentQuery = source.AsDocumentQuery();
                var results = new PagedResults<T>();
                try
                {
                    var queryResult = await documentQuery.ExecuteNextAsync<T>();
                    if (!queryResult.Any())
                    {
                        return results;
                    }
                    results.ContinuationToken = queryResult.ResponseContinuation;
                    results.Results.AddRange(queryResult);
                }
                catch
                {
                    //documentQuery.ExecuteNextAsync throws an exception on empty queries
                    return results;
                }
    
                return results;
            }
    }


## Update a document (.NET)
DocumentDB supports replacing JSON documents using the `ReplaceDocumentAsync` method of the `DocumentClient` class.

    await client.ReplaceDocumentAsync(UriFactory.CreateDocumentUri(databaseName, collectionName, familyName), updatedFamily);


## Delete a document (.NET)
DocumentDB supports deleting JSON documents using the `DeleteDocumentAsync` method of the `DocumentClient` class.

    await client.DeleteDocumentAsync(UriFactory.CreateDocumentUri(databaseName, collectionName, documentName));

## Delete a database (.NET)
Deleting a database will remove the database and all children resources (collections, documents, etc.).

    await this.client.DeleteDatabaseAsync(UriFactory.CreateDatabaseUri(databaseName));

