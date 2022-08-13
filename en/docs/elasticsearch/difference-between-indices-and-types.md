---
title: "Difference Between Indices and Types"
slug: "difference-between-indices-and-types"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

It's easy to see `type`s like a table in an SQL database, where the `index` is the SQL database. However, that is not a good way to approach `type`s.

## All About Types

In fact, types are _literally_ just a metadata field added to each document by Elasticsearch: `_type`. The examples above created two types: `my_type` and `my_other_type`. That means that each document associated with the types has an extra field automatically defined like `"_type": "my_type"`; this is indexed with the document, thus making it a _searchable or filterable field_, but it does not impact the raw document itself, so your application does not need to worry about it.

All types live in the same index, and therefore in the same collective shards of the index. Even at the disk level, they live in the same files. The only separation that creating a second type provides is a logical one. Every type, whether it's unique or not, needs to exist in the mappings and all of those mappings must exist in your cluster state. This eats up memory and, if each type is being updated dynamically, it eats up performance as the mappings change.

As such, it is a best practice to define only a single type unless you actually need other types. It is common to see scenarios where multiple types are desirable. For example, imagine you had a car index. It may be useful to you to break it down with multiple types:

- bmw
- chevy
- honda
- mazda
- mercedes
- nissan
- rangerover
- toyota
- ...

This way you can search for all cars or limit it by manufacturer on demand. The difference between those two searches are as simple as:

    GET /cars/_search

and

    GET /cars/bmw/_search

What is not obvious to new users of Elasticsearch is that the second form is a specialization of the first form. It literally gets rewritten to:

    GET /cars/_search
    {
      "query": {
        "bool": {
          "filter": [
            {
              "term" : {
                "_type": "bmw"
              }
            }
          ]
        }
      }
    }

It simply filters out any document that was not indexed with a `_type` field whose value was `bmw`. Since every document is indexed with its type as the `_type` field, this serves as a pretty simple filter. If an actual search had been provided in either example, then the filter would be added to the full search as appropriate.

As such, if the types are identical, it's much better to supply a single type (e.g., `manufacturer` in this example) and effectively ignore it. Then, within each document, explicitly supply a field called `make` _or whatever name you prefer_ and manually filter on it whenever you want to limit to it. This will reduce the size of your mappings to `1/n` where `n` is the number of separate types. It does add another field to each document, at the benefit of an otherwise simplified mapping.

In Elasticsearch 1.x and 2.x, such a field should be defined as

    PUT /cars
    {
      "manufacturer": { <1>
        "properties": {
          "make": { <2>
            "type": "string",
            "index": "not_analyzed"
          }
        }
      }
    }

1. The name is arbitrary.
2. The name is arbitrary _and_ it could match the type name if you wanted it too.

In Elasticsearch 5.x, the above will still work (it's deprecated), but the better way is to use:

    PUT /cars
    {
      "manufacturer": { <1>
        "properties": {
          "make": { <2>
            "type": "keyword"
          }
        }
      }
    }

1. The name is arbitrary.
2. The name is arbitrary _and_ it could match the type name if you wanted it too.

Types should be used sparingly within your indices because it bloats the index mappings, usually without much benefit. You must have at least one, but there is nothing that says you must have more than one.

## Common Questions

- What if I have two (or more) types that are mostly identical, but which have a few unique fields per type?

At the index level, there is no difference between one type being used with a few fields that are sparsely used _and_ between multiple types that share a bunch of non-sparse fields with a few not shared (meaning the other type never even uses the field(s)).

Said differently: a sparsely used field is sparse across the index _regardless of types_. The sparsity does not benefit -- or really hurt -- the index just because it is defined in a separate type.

You should just combine these types and add a separate type field.

- Why do separate types need to define fields in the exact same way?

Because each field is really only defined once at the Lucene level, regardless of how many types there are. The fact that types exist at all is a feature of Elasticsearch and it is _only_ a logical separation.

- Can I define separate types with the same field defined differently?

No. If you manage to find a way to do so in ES 2.x or later, then [you should open up a bug report](https://github.com/elastic/elasticsearch/issues). As noted in the previous question, Lucene sees them all as a single field, so there is no way to make this work appropriately.

ES 1.x left this as an implicit requirement, which allowed users to create conditions where one shard's mappings in an index actually differed from another shard in the same index. This was effectively a race condition and it _could_ lead to unexpected issues.

## Exceptions to the Rule

- Parent/child documents **require** separate types to be used within the same index.
    - The parent lives in one type.
    - The child lives in a separate type (but each child lives in the same _shard_ as its parent).
- Extremely niche use cases where creating tons of indices is undesirable and the impact of sparse fields is preferable to the alternative.
    - For example, the Elasticsearch monitoring plugin, Marvel (1.x and 2.x) or X-Pack Monitoring (5.x+), monitors Elasticsearch itself for changes in the cluster, nodes, indices, specific indices (the index level), and even shards. It could create 5+ indices each day to isolate those documents that have unique mappings _or_ it could go against best practices to reduce cluster load by sharing an index (note: the number of defined mappings is effectively the same, but the number of created indices is reduced from `n` to 1).
    - This is an advanced scenario, but you must consider the shared field definitions across types!

## Explicitly creating an Index with a Type
Example uses basic HTTP, which translate easily to cURL and other HTTP applications. They also match the [Sense](https://www.elastic.co/guide/en/sense/current/installing.html) syntax, which will be renamed to Console in Kibana 5.0.

Note: The example inserts `<#>` to help draw attention to parts. Those should be removed if you copy it!

    PUT /my_index <1>
    {
      "mappings": {
        "my_type": { <2>
          "properties": {
            "field1": {
              "type": "long"
            },
            "field2": {
              "type": "integer"
            },
            "object1": {
              "type": "object",
              "properties": {
                "field1" : {
                  "type": "float"
                }
              }
            }
          }
        }
      },
      "my_other_type": {
        "properties": {
          "field1": {
            "type": "long" <3>
          },
          "field3": { <4>
            "type": "double"
          }
        }
      }
    }

1. This is creating the `index` using the create index endpoint.
2. This is creating the `type`.
3. Shared fields in `type`s within the same `index` **must** share the same definition! ES 1.x did not strictly enforce this behavior, but it was an implicit requirement. ES 2.x and above strictly enforce this behavior.
4. Unique fields across `type`s are okay.

Indexes (or indices) _contain_ types. Types are a convenient mechanism for separating documents, but they require you to define -- either dynamically/automatically or explicitly -- a mapping for each type that you use. If you define 15 types in an index, then you have 15 unique mappings.

See the remarks for more details about this concept and why you may or may not want to use types.

## Dynamically creating an Index with a Type
Example uses basic HTTP, which translate easily to cURL and other HTTP applications. They also match the [Sense](https://www.elastic.co/guide/en/sense/current/installing.html) syntax, which will be renamed to Console in Kibana 5.0.

Note: The example inserts `<#>` to help draw attention to parts. Those should be removed if you copy it!

    DELETE /my_index <1>
    
    PUT /my_index/my_type/abc123 <2>
    {
      "field1" : 1234, <3>
      "field2" : 456,
      "object1" : {
        "field1" : 7.8 <4>
      }
    }

1. In case it already exists (because of an earlier example), delete the index.
2. Index a document into the index, `my_index`, with the type, `my_type`, and the ID `abc123` (could be numeric, but it is always a string).
    - By default, dynamic index creation is enabled by simply indexing a document. This is great for development environments, but it is not necessarily good for production environments.
3. This field is an integer number, so the first time it is seen it must be mapped. Elasticsearch always assumes the _widest_ type for any incoming type, so this would be mapped as a `long` rather than an `integer` or a `short` (both of which could contain `1234` and `456`).
4. The same is true for this field as well. It will be mapped as a `double` instead of a `float` as you might want.

This dynamically created index and type roughly match the mapping defined in the first example. However, it's critical to understand how `<3>` and `<4>` impact the automatically defined mappings.

You could follow this by adding yet another type dynamically to the same index:

    PUT /my_index/my_other_type/abc123 <1>
    {
      "field1": 91, <2>
      "field3": 4.567
    }

1. The type is the only difference from the above document. The ID is the same and that's okay! It has no relationship to the other `abc123` other than that it _happens_ to be in the same index.
2. `field1` already exists in the index, so it _must_ be the same type of field as defined in the other types. Submitting a value that was a string or not an integer would fail (e.g., `"field1": "this is some text"` or `"field1": 123.0`).

This would dynamically create the mappings for `my_other_type` within the same index, `my_index`.

Note: It is _always_ faster to define mappings upfront rather than having Elasticsearch dynamically perform it at index time.

The end result of indexing both documents would be similar to the first example, but the field types would be different and therefore slightly wasteful:

    GET /my_index/_mappings <1>
    {
      "mappings": {
        "my_type": { <2>
          "properties": {
            "field1": {
              "type": "long"
            },
            "field2": {
              "type": "long" <3>
            },
            "object1": {
              "type": "object",
              "properties": {
                "field1" : {
                  "type": "double" <4>
                }
              }
            }
          }
        }
      },
      "my_other_type": { <5>
        "properties": {
          "field1": {
            "type": "long"
          },
          "field3": {
            "type": "double"
          }
        }
      }
    }

1. This uses the `_mappings` endpoint to get the mappings from the index that we created.
2. We dynamically created `my_type` in the first step of this example.
3. `field2` is now a `long` instead of an `integer` because we did not define it upfront. This _may_ prove to be wasteful in disk storage.
4. `object1.field1` is now a `double` for the same reason as #3 with the same ramifications as #3.
    - Technically, a `long` can be compressed in a lot of cases. However, a `double` cannot be compressed due to it being a floating point number.
5. We also dynamically created `my_other_type` in the second step of this example. Its mapping happens to be the same because we were already using `long` and `double`.
    - Remember that `field1` _must_ match the definition from `my_type` (and it does).
    - `field3` is unique to this type, so it has no such restriction.

