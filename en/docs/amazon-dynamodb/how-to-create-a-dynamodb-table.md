---
title: "How to create a DynamoDB Table"
slug: "how-to-create-a-dynamodb-table"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

When creating tables make sure to pay attention to the choice of attributes for the partition and sort keys. See the published [guidelines for working with tables][1].


  [1]: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GuidelinesForTables.html

## Create Table in Java using Document API
In the following example we will be creating a table called `Membership` using the AWS Java SDK for DynamoDB. The table will consist of items that represent team assignments. The table will be partitioned by TeamID. Each team will have multiple members, identified by the MemberID (as a sort key).

    AWSCredentials credentials = new BasicAWSCredentials("access_key", "secret_key");
    DynamoDB dynamoDB = new DynamoDB(new AmazonDynamoDBClient(credentials));

    try {
          // every DynamoDB table must have basic schema that determines
          //   which attributes are to be used as partition key (and optionally sort key) 
      List<KeySchemaElement> keySchema = new ArrayList<~>();
          // required: specify the partition key (also called hash key)
      keySchema.add(new KeySchemaElement()
                 .withAttributeName("TeamID")
                 .withKeyType(KeyType.HASH)); 
          // optionally: add a sort key (also called a range key)
      keySchema.add(new KeySchemaElement()
                 .withAttributeName("MemberID")
                 .withKeyType(KeyType.RANGE));

          // after defining the key schema - the attributes that will be used as partition and range key
          //  we need to specify these attributes' type
      List<AttributeDefinition> attrDefinitions = new ArrayList<~>();
          //  we must specify the type for the TeamID attribute; suppose it's a string
      attrDefinitions.add(new AttributeDefinition()
                      .withAttributeName("TeamID")
                      .withAttributeType("S"));
          //  if using a sort key we need to specify its type; suppose that it's numeric
      attrDefinitions.add(new AttributeDefinition()
                      .withAttributeName("MemberID")
                      .withAttributeType("N"));

            
          // after defining the attributes making up the schema and their type
          // we build a request, specifying the table name and the provisioned capacity 
      CreateTableRequest request = new CreateTableRequest()
          .withTableName("Membership")
          .withKeySchema(keySchema)
          .withAttributeDefinitions(attrDefinitions)
          .withProvisionedThroughput(new ProvisionedThroughput()
               .withReadCapacityUnits(30L)
               .withWriteCapacityUnits(10L));

          // now submit the request to create the table in DynamoDB
      Table table = dynamoDB.createTable(request);

          // creating a table is an asynchronous operation
          // we can wait for the table to be created
      table.waitForActive();

          // the table is now ready and can be used

    } catch (Exception e) {
          // the table creation failed. 
          //   the reason for failure may be determined from the exception
    }

## Create Table in Ruby using AWS SDK v2
In following example we will create table `movies` with AWS Ruby SDK v2.
Here, each Movie as one unique Partition Key as `id`, and Range Key `year`. Apart from this we want to be able to query movies with their name, hence we will create a Global Secondary Index (GSI) `name-year-index` with `name` as Hash Key and `year`  as Range Key. Movie can have other attributes such as `released`, `created_at`, `actor` and `actress`. Schema for the table is shown below:

**Table Name**: movies

| Partition Key | Range Key | Global Secondary Index | Attributes |
| ------ | ------ | ------ | ------ |
| id   | year   | name | released , created_at, actor, actress |




    # it's better to initialize client as global variable in initializer
    $ddb ||= Aws::DynamoDB::Client.new({
      access_key_id: ENV["AWS_ACCESS_KEY_ID"],
      secret_access_key: ENV["AWS_SECRET_ACCESS_KEY"]
    })
    
    # create table API
    $ddb.create_table({

      # array of attributes name and their type that describe schema for the Table and Indexes
      attribute_definitions: [
        {
          attribute_name: "id",
          attribute_type: "N"
        }
        {
          attribute_name: "name",
          attribute_type: "S",
        },
        {
          attribute_name: "year",
          attribute_type: "N",
        }
      ],

      # key_schema specifies the attributes that make up the primary key for a table
      # HASH - specifies Partition Key
      # RANGE - specifies Range Key  
      # key_type can be either HASH or RANGE  
      key_schema: [
        {
          attribute_name: "id",
          key_type: "HASH",
        },
        {
          attribute_name: "year",
          key_type: "RANGE",
        }
      ],

      # global_secondary_indexes array specifies one or more keys that makes up index, with name of index and provisioned throughput for global secondary indexes
      global_secondary_indexes: [
        
        index_name: "name-year-index",
        key_schema: [
            {
                attribute_name: "name",
                key_type: "HASH"
            },
            {
                attribute_name: "year",
                key_type: "RANGE"
            }
        ],

        # Projection - Specifies attributes that are copied (projected) from the table into the index.
        # Allowed values are - ALL, INCLUDE, KEYS_ONLY
        # KEYS_ONLY - only the index and primary keys are projected into the index.
        # ALL - All of the table attributes are projected into the index.
        # INCLUDE - Only the specified table attributes are projected into the index. The list of projected attributes are then needs to be specified in non_key_attributes array
        projection: {
            projection_type: "ALL"
        },
        
        # Represents the provisioned throughput settings for specified index.
        provisioned_throughput: {
            read_capacity_units: 1,
            write_capacity_units: 1
        }
      ],  

      # Represents the provisioned throughput settings for specified table.  
      provisioned_throughput: {
        read_capacity_units: 1,
        write_capacity_units: 1,
      },
      table_name: "movies"
    })

    # wait till table is created
    $ddb.wait_until(:table_exists, {table_name: "movies"})



