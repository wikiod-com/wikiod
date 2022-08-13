---
title: "Java SDK"
slug: "java-sdk"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Adding the SDK to a project
Add the following dependency to your pom.xml file:

    <dependency>
      <groupId>com.couchbase.client</groupId>
      <artifactId>java-client</artifactId>
      <version>2.3.1</version>
    </dependency>

You can check the [maven repository][1] for the latest version.

If you want to use the Spring OEM use this Gradle dependency:

    compile(group: 'org.springframework.data', name: 'spring-data-couchbase', version: '2.1.6.RELEASE')

Replace the version with your desired release. Note that this is NOT the version of Couchbase but the version of the Spring connector.


  [1]: https://mvnrepository.com/artifact/com.couchbase.client/java-client

## Connecting to a Bucket
    String bucketName = "default";
    String bucketPassword = "";
    List<String> nodes = Arrays.asList("127.0.0.1"); // IP or hostname of one or more nodes in the cluster
    
    Cluster cluster = CouchbaseCluster.create(nodes);
    Bucket bucket = cluster.openBucket(bucketName, bucketPassword);

## Checking for document exists in DB
    String bucketName = "bucket";
    List<String> nodes = Arrays.asList("node1","node2"); // IP or hostname of one or more nodes in the cluster
    
    Cluster cluster = CouchbaseCluster.create(nodes);
    Bucket bucket = cluster.openBucket(bucketName);
    
    //check for a document by its ID
    
    String id="bucket_collection_user_123456";//document id
    boolean exists=bucket.exists(id);
    if(exists){
      System.out.println("Docuemnt exists");
    } 
    bucket.close();
    cluster.disconnect();



## Creating document with TTL (Time To Live)
   TTL value can be used to decide for how long the document needs to be there in the bucket. By default TTL value is 0, which means it will be there for indefinite time period.

    String bucketName = "bucket";
    List<String> nodes = Arrays.asList("node1","node2"); // IP or hostname of one or more nodes in the cluster
    
    Cluster cluster = CouchbaseCluster.create(nodes);
    Bucket bucket = cluster.openBucket(bucketName);
    //create the document with id 123 and TTL 1seconds
    bucket.insert(JsonDocument.create("123",JsonObject.empty(), 1)); //if TTL is 0 document will be there in the DB for indefinite time
    //do other stuffs
    //to update the TTL 
    bucket.upsert(JsonDocument.create("123",JsonObject.empty())); //no TTL value is provided 
    bucket.close();
    cluster.disconnect();



