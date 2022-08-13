---
title: "Sorted Sets"
slug: "sorted-sets"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The Sorted Set datatype in Redis is an ordered version of the Set datatype.  A Redis sorted set consists of a collection of unique members.  Each member in the sorted set can be thought of as a pair consisting of the member and a score.  The score is used to order the members within the set in ascending order.

## Syntax
- ZADD key [NX|XX] [CH] [INCR] score member [score member ...]
- ZCARD key
- ZCOUNT key min max
- ZLEXCOUNT key min max




The official documentation for Sorted Sets can be found at the [Redis.io](https://redis.io/commands#sorted_set) site.

Sorted sets are sometimes referred to as zsets.  If you use the TYPE command on a sorted set key, the value zset will be returned.


## Adding Items to a Sorted Set
Redis provides the ZADD command to add items to a sorted set.  The basic form of the ZADD command is to specify the set, the item to add and it's score.  For example, if I wanted to construct an ordered set of my favorite food (from least to most), I could use either of:
```
zadd favs 1 apple
zadd favs 2 pizza
zadd favs 3 chocolate
zadd favs 4 beer
```
or alternatively:
```
zadd favs 1 apple 2 pizza 3 chocolate 4 beer
```
The ZADD function operates very similarly to the unsorted set function SADD.  The result of the ZADD command is the number of items that were added.  So after creating my set as above, if I attempted to ZADD beer again:
```
ZADD favs 4 beer
```
I would get a 0 result, if I decided I like chocolate better than beer, I could execute:
```
ZADD favs 3 beer 4 chocolate
```
to update my preferences, but I would still get a 0 return result since both beer and chocolate are already in the set. 

## Counting Items in a Sorted Set
Redis provides three commands to count the items within a sorted set: ZCARD, ZCOUNT, ZLEXCOUNT.

The ZCARD command is the basic test for the cardinality of a set.  (It is analogous to the SCARD command for sets.) . ZCARD returns the count of the members of a set.
Executing the following code to add items to a set:
 

    zadd favs 1 apple
    zadd favs 2 pizza
    zadd favs 3 chocolate
    zadd favs 4 beer

running ZCard:

    zcard favs

returns a value of 4.

The ZCOUNT and ZLEXCOUNT commands allow you to count a subset of the items in a sorted set based on a range of values.  ZCOUNT allows you to count items within a particular range of scores and ZLEXCOUNT allowes you to count the number of items within a particular lexographic range.

Using our set above:

    zcount favs 2 5

would return a 3, since there are three items (pizza, chocolate, beer) that have scores between 2 and 5 inclusive.

ZLEXCOUNT is designed to work with sets where every item has the same score, forcing and ordering on the elemement names.  If we created a set like:

    zadd favs 1 apple
    zadd favs 1 pizza
    zadd favs 1 chocolate
    zadd favs 1 beer

we could use ZLEXCOUNT to get the number of elements in particular lexographical range (this is done by byte-wise comparison using the memcpy function).  

    zlexcount favs [apple (chocolate

would return 2, since two elements (apple, beer) fall within the range apple (inclusive) and chocolate (exclusive).  We could alternatively make both ends inclusive:

    zlexcount favs [apple [chocolate

and get the result 3.

