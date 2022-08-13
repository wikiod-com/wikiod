---
title: "Ektorp java client"
slug: "ektorp-java-client"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Test

## Opening a connection to CouchDB
    HttpClient httpClient = new StdHttpClient.Builder().
        url("http://yourcouchdbhost:5984").
        username("admin").
        password("password").
        build();

    CouchDbInstance dbInstance = new StdCouchDbInstance(httpClient);


## Simple CRUD with POJOs
One of the great things about Ektorp, is that it provides ORM like functionality, straight out of the box. This example will walk you through creating a simple POJO and doing standard CRUD operation on it


## Creating a simple POJO ##
----------
First off, we define a POJO as follows


    import com.fasterxml.jackson.annotation.JsonInclude;
    import com.fasterxml.jackson.annotation.JsonProperty;
    
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public class Person {
        @JsonProperty("_id") private String id;
        @JsonProperty("_rev") private String revision;
        private String name;
    
        public String getId() {
            return id;
        }
    
        public String getRevision() {
            return revision;
        }
    
        public String getName() {
            return name;
        }
    
    
        public void setId(String id) {
            this.id = id;
        }

        public void setRevision(String revision) {
            this.revision = revision;
        }
    
        public void setName(String name) {
            this.name = name;
        }
    }

So, whats happening here? The annotation `@JsonInclude(JsonInclude.Include.NON_NULL)` tells jackson not to serialize null fields into JSON. So, for example, if the id property is null you wont have the id property in the resulting JSON at all.

Additionally, the `@JsonProperty("_id")` and `@JsonProperty("_rev")` annotations are directives, informing the serializer/unserializer what JSON properties to map these values to. Documents in CouchDB must have both a _id and a _rev field, thus all POJOs which you intent to persist in CouchDB, must include a id and revision properties as above. You are free to name your properties differently in the POJO, as long as you don't change the annotations. For example,

    @JsonProperty("_id") private String identity;
    @JsonProperty("_rev") private String version;


## Persisting new instances to CouchDB ##
----------


Now, creating a brand new document, in the database is done as follows, presuming you have a valid CouchDbInstance instance, and that you wish to persist the document in a database named *person*

    CouchDbConnector connector = dbInstance.createConnector("person", true);
    
    Person person = new Person();
    person.setName("John Doe");
    connector.create(person);

Now, in this scenario, CouchDB will automatically create a new ID and Revision for you. If you dont want this, you can use

    connector.create("MyID", person);



## Loading, updating and deleting documents ##
----------
Presuming you have a CouchDBConnector instance ready, we can load instances of our POJO as follows

    Person person = connector.get(Person.class, "id");

then we can manipulate it, and update it as follows
    
    person.setName("Mr Bean");
    connector.update(person);

Notice, if the revision of the document in couch, doesn't match the revision of the document you are sending, the update will fail, and you need to load the latest version from the database and merge your instances accordingly.

and finally, should we wish to delete the instance, its as simple as

    connector.delete(person);


## Connecting to a database
Given you have a valid CouchDbInstance instance, you connect to a database within CouchDB in the following manner
    
    CouchDbConnector connector = dbInstance.createConnector("databaseName", true);

