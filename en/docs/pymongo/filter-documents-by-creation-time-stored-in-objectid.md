---
title: "Filter documents by creation time stored in ObjectId"
slug: "filter-documents-by-creation-time-stored-in-objectid"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Includes pymongo query examples to filter documents by timestamp encapsulated in ObjectId


## Documents created in the last 60 seconds
How to find documents created 60 seconds ago

```
seconds = 60

gen_time = datetime.datetime.today() - datetime.timedelta(seconds=seconds)
dummy_id = ObjectId.from_datetime(gen_time)

db.CollectionName.find({"_id": {"$gte": dummy_id}})


```

If you're in a different timezone, you may need to offset the datetime to UTC

```
seconds = 60

gen_time = datetime.datetime.today() - datetime.timedelta(seconds=seconds)
# converts datetime to UTC
gen_time=datetime.datetime.utcfromtimestamp(gen_time.timestamp())

dummy_id = ObjectId.from_datetime(gen_time)

db.Collection.find({"_id": {"$gte": dummy_id}})
```

