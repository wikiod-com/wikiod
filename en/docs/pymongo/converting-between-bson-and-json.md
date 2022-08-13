---
title: "Converting between BSON and JSON"
slug: "converting-between-bson-and-json"
draft: false
images: []
weight: 9861
type: docs
toc: true
---

In many applications, records from MongoDB need to be serialized in JSON format. If your records have fields of type date, datetime, objectId, binary, code, etc. you will encounter `TypeError: not JSON serializable` exceptions when using `json.dumps`. This topic shows how to overcome this.


## Using json_util
[json_util](http://api.mongodb.com/python/current/api/bson/json_util.html) provides two helper methods, `dumps` and `loads`, that wrap the native json methods and provide explicit BSON conversion to and from json. 

## Simple usage
  
<!-- language: lang-python -->
    from bson.json_util import loads, dumps
    record = db.movies.find_one()
    json_str = dumps(record)
    record2 = loads(json_str)

if `record` is:

<!-- language: lang-json -->
    { 
        "_id" : ObjectId("5692a15524de1e0ce2dfcfa3"), 
        "title" : "Toy Story 4", 
        "released" : ISODate("2010-06-18T04:00:00Z") 
    }
    
then `json_str` becomes:

<!-- language: lang-json -->
    {
        "_id": {"$oid": "5692a15524de1e0ce2dfcfa3"},
        "title" : "Toy Story 4", 
        "released": {"$date": 1276833600000}
    }

## JSONOptions

It is possible to customize the behavior of `dumps` via a `JSONOptions` object. Two sets of options are already available: `DEFAULT_JSON_OPTIONS` and `STRICT_JSON_OPTIONS`.

<!-- language: lang-python -->
    >>> bson.json_util.DEFAULT_JSON_OPTIONS
        JSONOptions(strict_number_long=False, datetime_representation=0,
         strict_uuid=False, document_class=dict, tz_aware=True, 
         uuid_representation=PYTHON_LEGACY, unicode_decode_error_handler='strict',
         tzinfo=<bson.tz_util.FixedOffset object at 0x7fc168a773d0>) 
    
To use different options, you can: 

1. modify the `DEFAULT_JSON_OPTIONS` object. In this case, the options will be used for all subsequent call to `dumps`:

    <!-- language: lang-python -->
        from bson.json_util import DEFAULT_JSON_OPTIONS
        DEFAULT_JSON_OPTIONS.datetime_representation = 2
        dumps(record)

2. specify a `JSONOptions` in a call to `dumps` using the `json_options` parameter:

    <!-- language: lang-python -->
        # using strict
        dumps(record, json_options=bson.json_util.STRICT_JSON_OPTIONS)

        # using a custom set of options
        from bson.json_util import JSONOptions
        options = JSONOptions() # options is a copy of DEFAULT_JSON_OPTIONS
        options.datetime_representation=2
        dumps(record, json_options=options)

The parameters of `JSONOptions` are:

* __strict_number_long__: If true, Int64 objects are encoded to MongoDB Extended JSON’s Strict mode type NumberLong, ie  `{"$numberLong": "<number>" }`. Otherwise they will be encoded as an int. Defaults to False.
* __datetime_representation__: The representation to use when encoding instances of datetime.datetime. 0 => `{"$date": <dateAsMilliseconds>}`, 1 => `{"$date": {"$numberLong": "<dateAsMilliseconds>"}}`, 2 => `{"$date": "<ISO-8601>"}`
* __strict_uuid__: If true, uuid.UUID object are encoded to MongoDB Extended JSON’s Strict mode type Binary. Otherwise it will be encoded as `{"$uuid": "<hex>" }`. Defaults to False.
* __document_class__: BSON documents returned by loads() will be decoded to an instance of this class. Must be a subclass of collections.MutableMapping. Defaults to dict.
* __uuid_representation__: The BSON representation to use when encoding and decoding instances of uuid.UUID. Defaults to PYTHON_LEGACY.
* __tz_aware__: If true, MongoDB Extended JSON’s Strict mode type Date will be decoded to timezone aware instances of datetime.datetime. Otherwise they will be naive. Defaults to True.
* __tzinfo__: A `datetime.tzinfo` subclass that specifies the timezone from which datetime objects should be decoded. Defaults to utc.


## Using the json module with custom handlers
If all you need is serializing mongo results into json, it is possible to use the `json` module, provided you define custom handlers to deal with non-serializable fields types.
One advantage is that you have full power on how you encode specific fields, like the datetime representation.


Here is a handler which encodes dates using the iso representation and the id as an hexadecimal string:

<!-- language: lang-python --> 

    import pymongo
    import json 
    import datetime
    import bson.objectid

    def my_handler(x):
        if isinstance(x, datetime.datetime):
            return x.isoformat()
        elif isinstance(x, bson.objectid.ObjectId):
            return str(x)
        else:
            raise TypeError(x)

    db = pymongo.MongoClient().samples
    record = db.movies.find_one()
    # {u'_id': ObjectId('5692a15524de1e0ce2dfcfa3'), u'title': u'Toy Story 4',
    #   u'released': datetime.datetime(2010, 6, 18, 4, 0),}

    json_record = json.dumps(record, default=my_handler)
    # '{"_id": "5692a15524de1e0ce2dfcfa3", "title": "Toy Story 4", 
    #    "released": "2010-06-18T04:00:00"}'

## Using python-bsonjs
[python-bsonjs](https://pypi.python.org/pypi/python-bsonjs) does not depend on PyMongo and can offer a nice performance improvement over `json_util`:

> [bsonjs](https://pypi.python.org/pypi/python-bsonjs) is roughly 10-15x faster than PyMongo’s json_util at decoding BSON to JSON and encoding JSON to BSON.

Note that:

 - to use bsonjs effectively, it is recommended to work directly with [`RawBSONDocument`](http://api.mongodb.com/python/current/api/bson/raw_bson.html)
 - dates are encoded using the LEGACY representation, i.e. `{"$date": <dateAsMilliseconds>}`. There is currently no options to change that.


## Installation

    pip install python-bsonjs

## Usage

To take full advantage of the bsonjs, configure the database to use the `RawBSONDocument` class. Then, use `dumps` to convert  bson raw bytes to json and `loads` to convert json to bson raw bytes: 

<!-- language: lang-python -->

    import pymongo
    import bsonjs
    from pymongo import MongoClient
    from bson.raw_bson import RawBSONDocument

    # configure mongo to use the RawBSONDocument representation
    db = pymongo.MongoClient(document_class=RawBSONDocument).samples
    # convert json to a bson record
    json_record = '{"_id": "some id", "title": "Awesome Movie"}' 
    raw_bson = bsonjs.loads(json_record)
    bson_record = RawBSONDocument(raw_bson)
    # insert the record
    result = db.movies.insert_one(bson_record)
    print(result.acknowledged)

    # find some record
    bson_record2 = db.movies.find_one()
    # convert the record to json
    json_record2 = bsonjs.dumps(bson_record2.raw)
    print(json_record2)


