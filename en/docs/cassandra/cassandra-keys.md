---
title: "Cassandra keys"
slug: "cassandra-keys"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Partition key, clustering key, primary key
Cassandra uses two kinds of keys:

 - the __Partition Keys__ is responsible for data distribution across nodes
 - the __Clustering Key__ is responsible for data sorting within a partition

A __primary key__ is a combination of those to types. The vocabulary depends on the combination:

 - __simple primary key__: only the partition key, composed of one column
 - __composite partition key__: only the partition key, composed of multiple columns
 - __compound primary key__: one partition key with one or more clustering keys.
 - __composite and compound primary key__: a partition key composed of multiple columns and multiple clustering keys.

## The PRIMARY KEY syntax
## Declaring a key 

The table creation statement should contain a `PRIMARY KEY` expression. The way you declare it is very important. In a nutshell:

    PRIMARY KEY(partition key)
    PRIMARY KEY(partition key, clustering key)

Additional parentheses group multiple fields into a composite partition key or declares a compound composite key.

## Examples


__Simple__ primary key: 

    PRIMARY KEY (key)

`key` is called the __partition key__.

(for simple primary key, it is also possible to put the `PRIMARY KEY` expression after the field, i.e. `key int PRIMARY KEY,` for example).

__Compound__ primary key:

    PRIMARY KEY (key_part_1, key_part_2)

Contrary to SQL, this does not exactly create a composite primary key. Instead, it declares `key_part_1` as the __partition key__ and `key_part_2` as the __clustering key__. Any other field will also be considered part of the clustering key.

__Composite+Compound__ primary keys:

    PRIMARY KEY ((part_key_1, ..., part_key_n), (clust_key_1, ..., clust_key_n))

The first parenthese defines the __compound partition key__, the other columns are the clustering keys.

## Syntax summary

- `(part_key)`
- `(part_key, clust_key)`
- `(part_key, clust_key_1, clust_key_2)`
- `(part_key, (clust_key_1, clust_key_2))`
- `((part_key_1, part_key_2), clust_key)`
- `((part_key_1, part_key_2), (clust_key_1, clust_key_2))`

## Key ordering and allowed queries
The __partition key__ is the __minimum specifier__ needed to perform a query using a where clause. 

If you declare a __composite clustering key__, the order matters.

Say you have the following primary key:

    PRIMARY KEY((part_key1, part_key_2), (clust_key_1, clust_key_2, clust_key_3))

Then, the _only valid queries_ use the following fields in the `where` clause:

-  `part_key_1`, `part_key_2`
-  `part_key_1`, `part_key_2`, `clust_key_1`
-  `part_key_1`, `part_key_2`, `clust_key_1`, `clust_key_2`
-  `part_key_1`, `part_key_2`, `clust_key_1`, `clust_key_2`, `clust_key_3`

Example of invalid queries are:

- `part_key_1`, `part_key_2`, `clust_key_2`
- Anything that does not contain both `part_key_1`, `part_key_2`
- ...

If you want to use `clust_key_2`, you have to also specify `clust_key_1`, and so on. 

So the order in which you declare your clustering keys will have an impact on the type of queries you can do.
In the opposite, the order of the partition key fields is not important, since you always have to specify all of them in a query.



