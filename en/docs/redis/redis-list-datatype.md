---
title: "Redis List Datatype"
slug: "redis-list-datatype"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The List datatype in Redis is an ordered collection of items referenced by a Redis key.  Redis allows you to access and modify a list by index or push/pop operations.  In Redis, the two ends of a list are referred to as the left and the right.  The left corresponds to the first element or head of a list and the right coresponds to the last element or tail of a list.

## Syntax
- LPUSH key value [value ...]
- RPUSH key value [value ...]
- LPOP key
- RPOP key
- LLEN key


More detail on the List datatype and all the commands that can be used in conjunction with them can be found in the official Redis documentation at [Redis.io](https://redis.io/commands#list).

## Adding Items to a List
Redis allows you to add items to either the right or the left of a list.

If I was working with a list, my_list and I wanted to prepend 3 to the list, I could do that using the Redis LPUSH command:
```
LPUSH my_list 3
```
If I wanted to append 3 to my_list, I would instead use the RPUSH command:
```
RPUSH my_list 3
```
Both the LPUSH and RPUSH command will automatically create a new list for you if the supplied key doesn't exist.  Two alternative commands LPUSHX and RPUSHX can be used to only operate on the list key if it already exists.

## Getting Items from a List
Redis provides the LPOP and RPOP commands as a counterpart to the LPUSH and RPUSH commands for fetching data items.

If I was working with a list my_list that had several data items in it already, I can get the first item in the list using the LPOP command:
```
LPOP my_list
```
The result of this command will return the value of the first element from the list and remove it from my_list.  For example, if I had the list [1, 3, 2, 4] and I applied LPOP to it, I would have the list [3, 2, 4] in memory afterwards.

Similarly, I can remove from the end of the list using RPOP:
```
RPOP my_list
```
would return the value fo the last element form the list and then remove it from my_list.  Using our example, [1, 2, 3, 4] after calling RPOP on this list, the list in memory would be [1, 2, 3].

## Size of a List
The size of a Redis list can be deterimed using the LLEN command.  If I have a four element list stored at the key my_list, I can get the size using:
```
LLEN my_list
```
which will return 4.

If a user specifies a key that doesn't exist to LLEN, it will return a zero, but if a key is used that points to an item of a different datatype, an error will be returned.

