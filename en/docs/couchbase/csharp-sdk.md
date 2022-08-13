---
title: "C# SDK"
slug: "c-sdk"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - new Cluster(); // Connect and bootstrap from the local cluster (localhost)
 - new Cluster(ClientConfiguration configuration) // Use the specified custom configuration to connect to the cluster
 - new Cluster(string configurationSectionName) // Use the specified configuration section from app.config / web.config
 - OpenBucket(); // Open the default bucket
 - OpenBucket(string bucketname); // Open the specified bucket
 - OpenBucket(string bucketname, string password); // Open the specified bucket with the provided password


## Connecting to a Bucket
Configuring the connection programmatically:


    var config = new ClientConfiguration
            {
                Servers = new List<Uri> {
                    new Uri("http://localhost:8091/pools")
                },
                BucketConfigs = new Dictionary<string, BucketConfiguration>
                  {
                    { "default", new BucketConfiguration
                    {
                      BucketName = "default",
                      UseSsl = false,
                      Password = "",
                      DefaultOperationLifespan = 2000,
                      PoolConfiguration = new PoolConfiguration
                      {
                        MaxSize = 10,
                        MinSize = 5,
                        SendTimeout = 12000
                      }
                    }}
                  }
            };

    var cluster = new Cluster(config);
    var bucket = cluster.OpenBucket();


Configuring the connection in `web.config` / `app.config`:

    <?xml version="1.0" encoding="utf-8" ?>
    <configuration>
        <configSections>
            <sectionGroup name="couchbaseClients">
                <section name="couchbase" type="Couchbase.Configuration.Client.Providers.CouchbaseClientSection, Couchbase.NetClient" />
            </sectionGroup>
        </configSections>
        <couchbaseClients>
            <couchbase useSsl="false">
                <servers>
                    <add uri="http://localhost:8091/pools"></add>
                </servers>
                <buckets>
                    <add name="default" useSsl="false" password="">
                        <connectionPool name="custom" maxSize="10" minSize="5" sendTimeout="12000" />
                    </add>
                </buckets>
            </couchbase>
        </couchbaseClients>
    </configuration>

Using the config section:

    var cluster = new Cluster("couchbaseClients/couchbase");
    var bucket = cluster.OpenBucket();


## Insert Document Sync
There are two basic ways in which you can Insert a document
1. Create a document, Then insert it
   

    var bucket = cluster.OpenBucket("default");

    var document = new Document<dynamic>
        {
            Id = "doc_net",
            Content = new
            {
                name = "Roi",
                lastName = "Katz",
                someRandomField="Very important data!"
            },
            Expiry = 0, // TTL in ms
        };
    
        bucket.Insert(document);

2. Use a Serialized object and Newtonsoft JSON.net


      public class MyDataObject
        {
            public string Name { get; set; }
            public int LastName { get; set; }
            public string SomeRandomField { get; set; }
        }

And Use it to insert your data 

    var dataObject = new MyDataObject();
    //...Fill up the object
    bucket.Insert("MyUniqueDocumentKey", dataObject, 10); // Insert a document with 10 seconds TTL - or you can use a TimeSpan

You can also sent persistence of replication factor while you insert the document.
[![Replication and persistence settings of the document][1]][1]

  [1]: http://i.stack.imgur.com/M0kgn.png

## Adding the SDK to a project
From the NuGet Package Manager console:

    Install-Package CouchbaseNetClient

