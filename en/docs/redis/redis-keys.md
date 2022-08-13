---
title: "Redis Keys"
slug: "redis-keys"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

The Redis keyspace can be thought of as a hash table or dictionary mapping keys to data structures in the database.

Redis provides a wide range of commands that work with keys to manage the keyspace, including the ability to remove keys, inspect key metadata, search for keys, and modify certain properties of keys.

## Syntax
- KEYS pattern
- PERSIST key
- EXPIRE key seconds
- EXPIREAT key timestamp
- TTL key
- PEXPIRE key milliseconds
- PEXPIREAT key milliseconds-timestamp
- PTTL key
- UNLINK key [key ...]
- DEL key [key ...]
- SCAN cursor [MATCH pattern] [COUNT count]
 

For *valid characters in Redis keys*, [the manual explains this completely](http://redis.io/topics/data-types-intro):

> Redis keys are binary safe, this means that you can use any binary sequence as a key, from a string like "foo" to the content of a JPEG file. The empty string is also a valid key.

>A few other rules about keys:

>Very long keys are not a good idea, for instance a key of 1024 bytes is a bad idea not only memory-wise, but also because the lookup of the key in the dataset may require several costly key-comparisons. Even when the task at hand is to match the existence of a large value, to resort to hashing it (for example with SHA1) is a better idea, especially from the point of view of memory and bandwidth.

>Very short keys are often not a good idea. There is little point in writing "u1000flw" as a key if you can instead write "user:1000:followers". The latter is more readable and the added space is minor compared to the space used by the key object itself and the value object. While short keys will obviously consume a bit less memory, your job is to find the right balance.

>Try to stick with a schema. For instance "object-type:id" is a good idea, as in "user:1000". Dots or dashes are often used for multi-word fields, as in "comment:1234:reply.to" or "comment:1234:reply-to".

>The maximum allowed key size is 512 MB.

> Be careful with using the KEYS command against a production system, it can cause serious performance problems.  If you need to do a search against the keyspace the [SCAN](https://redis.io/commands/scan) commands are a better alternative.

## Valid Keys
Redis keys are binary-safe, so literally anything can be used as a key. The only limitations are that they must be less than 512MB.

Examples of valid keys:

    7
    ++++
    `~!@#$%^&*()-_=+
    user:10134
    search/9947372/?query=this%20is%20a%28test%29%20query
    <div id="div64">

    Any other string less than 512MB in size.
    The raw binary content of an image or other binary file.
    An entire multi-line text document.
    An entire SQL query.
    Any integer, hexadecimal, octal, or binary value.
    Anything else you can think of less than 512MB in size. 

Invalid Redis keys:

    Anything larger than 512MB.
    

## Key Naming Schemes
For clarity and maintainability, it is often recommended to develop a system or schema for naming your Redis keys. Here are some examples of common and maintainable systems for naming your keys:

    user:10134
    user:10134:favorites
    user:10134:friends
    user:10134:friends-of-friends

    user:10134
    user:10134/favorites
    user:10134/friends
    user:10134/friends.of.friends

    user/10134
    user/10134/favorites
    user/10134/friends
    user/10134/friends of friends

Note that, while allowed, larger keys use more memory and result in slower lookup times, so using a 500MB key might not be a great idea for performance. A better idea might be to use a SHA-1, SHA-256, or MD5 hash of a large binary object as a key instead:

    image/9517bb726d33efdc503a43582e6ea2eea309482b
    image52e9df0577fca2ce022d4e8c86b1eccb070d37bef09dec36df2fabbfa7711f5c

## Listing all keys
You can list all of the keys in a Redis database by executing the following commands from redis-cli:

```
KEYS *
```

The parameter to KEYS is a glob-style pattern matching expression.  Examples of suppored patterns include:

```
h?llo matches hello, hallo and hxllo
h*llo matches hllo and heeeello
h[ae]llo matches hello and hallo, but not hillo
h[^e]llo matches hallo, hbllo, ... but not hello
h[a-b]llo matches hallo and hbllo
```

Using the KEYS * command can have adverse affects on performance, so it is not recommended against production instances.  Use the SCAN operation to search for keys in production code.

## TTL and Key Expiration
The expiration values of a key can be managed by a user outside of the update commands.  Redis allows a user to determine the current time to live (TTL) of a key using the TTL command:

    TTL key

This command will return the TTL of a key in seconds or will return the special values -1 or -2.  A -1 indicates that the key is persistent (won't expire) and a -2 indicates that the key does not exist.

An expiring key can be made persistent using the PERSIST command:

    PERSIST KEY

and a persistent key can be made to expire using the EXPIRE command:

    EXPIRE KEY seconds

Expire can also be used to modify the TTL of an existing key.  Alternatively, you can use the EXPIREAT command with a UNIX timestamp to set an expire time.

There are millisecond versions of TTL, EXPIRE and EXPIREAT commands that are prefixed with a P.



## Deleting Keys
Redis provides two functions for removing keys from the database: del and unlink.  

The del function removes one or more keys from the database.  The del command causes Redis to immediately reclaim the memory for the deleted key on the current thread of execution.  The execution time for del is proportional to the number of individual elements deleted from all the keys.

The unlink function acts like the del command, it removes one or more keys from the database.  However, unlike the del command, any memory used by those keys is reclaimed asynchronously on another thread.

## Scanning the Redis Keyspace
Redis provides the SCAN command to iterate over the keys in the database matching a
particular pattern. Redis supports glob style pattern matching in the SCAN command. 

The SCAN command provides a cursor-based iterator over the Redis keyspace.  The iterative call sequence to SCAN starts with the user making a call with the cursor argument set to 0.  The result of that call is a batch of items and an updated cursor which is supplied to the next call to SCAN.  This iteration continues until Redis returns a 0 cursor.

The following Python function demonstrates the basic usage of SCAN:
```
def scan_keys(r, pattern):
    "Returns a list of all the keys matching a given pattern"

    result = []
    cur, keys  = r.scan(cursor=0, match=pattern, count=2)
    result.extend(keys)
    while cur != 0:
        cur, keys = r.scan(cursor=cur, match=pattern, count=2)
        result.extend(keys)
        
    return result
```

The SCAN command is the recommended way to search for keys in the database, and is recommended over the `KEYS *` command.


