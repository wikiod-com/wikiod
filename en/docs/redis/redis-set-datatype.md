---
title: "Redis Set Datatype"
slug: "redis-set-datatype"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Redis supports a set datatype analogous to mathematical sets for modeling data in the database.  Sets are a compound datatype consisting of a group of unique, unordered members.  Sets support adding and removing members, size operations, as well as combination operations that take two sets and generate a third set.  Sets in Redis are similar to Sets in most programming languages.

## Syntax
- SADD key member [member ...]
- SISMEMBER key member
- SCARD key
- SADD key member [member ...]

The full documentation on the Redis set datatype can be found at [Redis.io](https://redis.io/commands#set).

## Size of a Set
The size of a set can be determined using the SCARD command.  SCARD will return the cardinality of a set or the number of members in the set.  For example, if I had a Redis set my_set stored in the database that looked like (Apple, Orange, Banana), I could get the size using the following code:
```
SCARD my_set
```
In the case of my example set, this would return 3.  If the user executes an SCARD command on a key that does not exist, Redis will return 0.

## Adding Items to a Set
The basic Redis command for adding an item to a set is SADD.  It takes a key and one or more members and adds them to the set stored at the given key.

For example, lets say that I wanted to create a set with the items apple, pear and banana.  I could execute either of the following:
```
SADD fruit apple
SADD fruit pear
SADD fruit banana
```
or
```
SADD fruit apple pear banana
```
After executing either, I will have the set fruit with 3 items.

Attempting to add an item that is already in the set will have no effect.  After setting up my fruit set using the code above, if I try to add apple again:
```
SADD fruit apple
```
Redis will attempt to add apple to the fruit set, but since it is already in the set nothing will change.

The result of the SADD command is always the number of items Redis added to a set.  So attempting to re-add apple, will return a result of 0.

Member items in Redis are case sensitive, so apple and Apple are treated as two separate items.

## Testing for Membership
Redis supplies the SISMEMBER command to test if a particular item is already a member of a set.  Using the SISMEMBER command I can test and see if apple is already a member of my fruit set.

If I construct my fruit set from the previous example, I can check and see if it contains apple using the following test:
```
SISMEMBER fruit apple
```
SISMEMBER will return a 1 since the item is already there.

If I tried to see if dog is a member of my fruit set:
```
SISMEMBER fruit dog
```
Redis will return a 0 since dog isn't in the fruit set.  

If a user attempts to use the SISMEMBER command with a key that doesn't exist, Redis will return a 0 indicating no membership, but if you use SISMEMBER with a key that already holds a non-set datatype, Redis will return an error.

