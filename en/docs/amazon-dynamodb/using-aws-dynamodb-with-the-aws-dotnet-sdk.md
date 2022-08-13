---
title: "Using AWS DynamoDb with the AWS .NET SDK"
slug: "using-aws-dynamodb-with-the-aws-net-sdk"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

[Amazon DynamoDB][1] is a fast NoSQL database service offered by Amazon Web Services (AWS). DynamoDB can be invoked from .NET applications by using the [AWS SDK for .NET][2]. The SDK provides three different models for communicating with DynamoDB. This topic is introduces the various APIs in each model.

The Models
----------

The SDK provides three ways of communicating with DynamoDB. Each one offers tradeoffs between control and ease of use. See the [AWS .NET SDK Reference][3] for details on the APIs below.

- **Low-level**: `Amazon.DynamoDBv2` namespace — This is a thin wrapper over the DynamoDB service calls. It matches all the service features. You can reference the [service documentation][4] to learn more about each individual operation.

- **Document Model**: `Amazon.DynamoDBv2.DocumentModel` namespace — This is a model that  provides a simpler interface for dealing with data. DynamoDB tables are represented by `Table` objects, while individual rows of data are represented by `Document` objects. Conversion of .NET objects to DynamoDB data is automatic for basic types.
  
- **Object Persistence Model**: `Amazon.DynamoDBv2.DataModel` namespace — This set of APIs allow you to store and load .NET objects in DynamoDB. Objects must be marked up to configure the target table and the hash/range keys. `DynamoDBContext` acts on marked up objects. It is used to store and load DynamoDB data, or to retrieve .NET objects from a query or scan operation. Basic data types are automatically converted to DynamoDB data and converters allow arbitrary types to be stored in DynamoDB.

The three models provide different approaches to working with the service. While the low-level approach requires more client-side code — the user must convert .NET types such as numbers and dates to DynamoDB-supported strings — it provides access to all service features. By comparison, the Object Persistence Model approach makes it easier to use the service—since the user is for the most part working with familiar .NET objects—but does not provide all the functionality. For example, it is not possible to make conditional Put calls with the Object Persistence Model.

Learn more about working AWS using the .NET SDK in the [.NET SDK Developer Guide][5].

*Note: This topic was adapted with permission from a blog post originally published on the [AWS .NET SDK blog][6].*


  [1]: http://aws.amazon.com/dynamodb/
  [2]: http://aws.amazon.com/sdk-for-net
  [3]: https://docs.aws.amazon.com/sdkfornet/v3/apidocs/Index.html
  [4]: http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html?page=NDynamoDBv2_DocumentModel_NET4_5.html&tocid=Amazon_DynamoDBv2_DocumentModel
  [5]: https://docs.aws.amazon.com/AWSSdkDocsNET/V3/DeveloperGuide/welcome.html
  [6]: https://blogs.aws.amazon.com/net/blog

## Low Level API Example
    var client = new AmazonDynamoDBClient();
     
    // Store item
    client.PutItem(new PutItemRequest
    {
        TableName = "Books",
        Item = new Dictionary<string, AttributeValue>
        {
            { "Title", new AttributeValue { S = "Cryptonomicon" } },
            { "Id", new AttributeValue { N = "42" } },
            { "Authors", new AttributeValue {
                SS = new List<string> { "Neal Stephenson" } } },
            { "Price", new AttributeValue { N = "12.95" } }
        }
    });
     
    // Get item
    Dictionary<string, AttributeValue> book = client.GetItem(new GetItemRequest
    {
        TableName = "Books",
        Key = new Dictionary<string, AttributeValue>
        {
            { "Id", new AttributeValue { N = "42" } }
        }
    }).Item;
     
    Console.WriteLine("Id = {0}", book["Id"].S);
    Console.WriteLine("Title = {0}", book["Title"].S);
    Console.WriteLine("Authors = {0}",
        string.Join(", ", book["Authors"].SS));

## Document Model Example
    var client = new AmazonDynamoDBClient();
    Table booksTable = Table.LoadTable(client, "Books");
     
    // Store item
    Document book = new Document();
    book["Title"] = "Cryptonomicon";
    book["Id"] = 42;
    book["Authors"] = new List<string> { "Neal Stephenson" };
    book["Price"] = 12.95;
    booksTable.PutItem(book);
     
    // Get item
    book = booksTable.GetItem(42);
    Console.WriteLine("Id = {0}", book["Id"]);
    Console.WriteLine("Title = {0}", book["Title"]);
    Console.WriteLine("Authors = {0}",
        string.Join(", ", book["Authors"].AsListOfString()));

## Object Persistence Model Example
This example consists of two parts: first, we must define our `Book` type; second, we use it with `DynamoDBContext`.

    [DynamoDBTable("Books")]
    class Book
    {
        [DynamoDBHashKey]
        public int Id { get; set; }
        public string Title { get; set; }
        public List<string> Authors { get; set; }
        public double Price { get; set; }
    }

Now, use it with `DynamoDBContext`.

    var client = new AmazonDynamoDBClient();
    DynamoDBContext context = new DynamoDBContext(client);
     
    // Store item
    Book book = new Book
    {
        Title = "Cryptonomicon",
        Id = 42,
        Authors = new List<string> { "Neal Stephenson" },
        Price = 12.95
    };
    context.Save(book);
     
    // Get item
    book = context.Load<Book>(42);
    Console.WriteLine("Id = {0}", book.Id);
    Console.WriteLine("Title = {0}", book.Title);
    Console.WriteLine("Authors = {0}", string.Join(", ", book.Authors));

