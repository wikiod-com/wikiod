---
title: "Batch Operations Things to know"
slug: "batch-operations-things-to-know"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Database is an integral part of any application and performance and persistance are real challenges faced by any web application. NoSql databases are no different in this matter and need to be dealt carefully. DynamoDB being one of the NoSQL database that is provided by Amazon Web Services support batch operations in addition to the CRUD operations.
Lets start with Batch Operations. In this example we will learn how we can make use of Dynamo DB's JAVA SDK to perform Batch Inserts.

**Good to know about Batch operations**
1) Batch operation doesn't minimize the HTTP request count, rather it has more features to handle when we receive a throttling error. To put it simple, each record that we insert into dynamo db will consume one http request.
2) Sound implementation of batch operation is to first to cache data temporarily and once we have the threshold number, ie 25, is the correct time to fire a batch request.
3) Any request that fails due to throttling will be retried between(500-999 ) milliseconds, as recommended by Amazon best practices.



## How to code the BatchWriteItemRequest and save data
    private static void saveItem(List<EventTracker> items) {
        List<WriteRequest> wrList = new ArrayList<>();
        try {

            for (EventTracker item : items) {
                WriteRequest wreqItem;
                wreqItem = getWriteRequest(item);
                wrList.add(wreqItem);
            }

            try {

                BatchWriteItemResult batchWriteItemResult = new BatchWriteItemResult();
                do {
                    BatchWriteItemRequest batchWriteItemRequest = new BatchWriteItemRequest();
                    batchWriteItemRequest.addRequestItemsEntry(forumTableName, wrList);// setRequestItems(writeRequestitems);
                    batchWriteItemResult = amazonDynamoDB.batchWriteItem(batchWriteItemRequest);
                    // Check for unprocessed keys which could happen if you
                    // exceed
                    // provisioned throughput
                    Map<String, List<WriteRequest>> unprocessedItems = batchWriteItemResult.getUnprocessedItems();
                    if (unprocessedItems.size() == 0) {
                        System.out.println("No unprocessed items found");
                    } else {
                        System.out.println("Sleeping for: " + ThreadLocalRandom.current().nextInt(500, 999 + 1));
                        Thread.sleep(ThreadLocalRandom.current().nextInt(500, 999 + 1));
                        wrList = unprocessedItems.get(forumTableName);
                        System.out.println("Retrieving the unprocessed items");
                    }

                } while (batchWriteItemResult.getUnprocessedItems().size() > 0);

            } catch (Exception e) {
                System.err.println("Failed to retrieve items: ");
                e.printStackTrace(System.err);
            }

        } catch (Exception e) {

        }
    }

This is what we need to know if we want to make use of batch operations to put data to a dynamo db table. Lets see the steps that need to be followed to accomplish this.
In this example, we are trying to persist a list of EventTracker data, which is my POJO.
1) For each record to be inserted, we need to create a PUT Request.
2) Each PUT Request is wrapped to a Write Request.
3) All Write Request are bundled into a List.
4) The WriteRequest List is then added to the BatchWriteItemRequest and executed.


The below code will show how we create write requests.


## How to create WriteRequest
    private static WriteRequest getWriteRequest(EventTracker event) {

        WriteRequest wreq = null;// = new WriteRequest();

        if (event != null) {
            Map<String, AttributeValue> attributeMap = new HashMap<String, AttributeValue>();

            addAttribute(attributeMap, "event_id", event.getEventId());
            addAttribute(attributeMap, "created_datetime", event.getCreatedDatetime());
            addAttribute(attributeMap, "event", event.getEvent());
            addAttribute(attributeMap, "event_type", event.getEventType());
            addAttribute(attributeMap, "response_id", "NULL");

            wreq = new WriteRequest(new PutRequest(attributeMap));
        }
        return wreq;
    }

    private static void addAttribute(Map<String, AttributeValue> item, String attributeName, String value) {
        AttributeValue attributeValue = new AttributeValue(value);
        item.put(attributeName, attributeValue);
    }

