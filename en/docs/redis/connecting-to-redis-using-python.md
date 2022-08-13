---
title: "Connecting to redis using Python"
slug: "connecting-to-redis-using-python"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

Connecting to Redis in Python requires the use of a client library.  Many different client libraries exist for Python, but **redis-py** is one of the most popular clients in use.

Once you install your client library, you can then access Redis in your application by importing the appropriate module, establishing a connection, then executing a command.

To connect on redis with python you need to install a **[client][1]**. 
You can install with pip using:

    pip install redis
this will install **[redis-py][2]**

Optionally, you may want to install **hiredis-py** which delegates parsing of protocol messages to the C hiredis client.  This can provide significant performance improvement in many situations.  You can install hiredis with pip by executing:

    pip install hiredis




  [1]: https://redis.io/clients#python
  [2]: https://github.com/andymccurdy/redis-py
  [3]: https://github.com/redis/hiredis-py


## Add element to list
    import redis

    r = redis.StrictRedis(host='localhost', port=6379, db=0)

    r.lpush('myqueue','myelement')


## Adding fields to a Hash
There are two main functions in Redis (HSET and HMSET) for adding fields to a hash key.  Both functions are available in redis-py.

Using HSET:
```
import redis

r = redis.StrictRedis(host='myserver', port=6379, db=0)
r.hset('my_key', 'field0', 'value0')
```

Using HMSET:
```
import redis

r = redis.StrictRedis(host='myserver', port=6379, db=0)
r.hmset('my_key', {'field0': 'value0', 'field1':'value1', 'field2':'value2'}
```

 

## Setting up a Connection to Redis
The **redis-py** client provides two classes `StrictRedis` and `Redis` to establish a basic connection to a Redis database.  The `Redis` class is provided for backwards compatibility and new projects should use the `StrictRedis` class.

One of the recommended ways to establish a connection, is to define the connection parameters in a dictionary and pass the dictionary to the `StrictRedis` constructor using the `**` syntax.

```
conn_params = {
    "host": "myredis.somedomain.com",
    "port": 6379,
    "password": "sekret",
    "db": 0
}

r = redis.StrictRedis(**config)
```

## Creating a transaction
You can establish a transaction by calling the `pipeline` method on the `StrictRedis`.  Redis commands executed against the transaction are performed in a single block.

    # defaults to transaction=True 
    tx = r.pipeline()
    tx.hincrbyfloat(debit_account_key, 'balance', -amount)
    tx.hincrbyfloat(credit_account_key, 'balance', amount)
    tx.execute()


## Executing Commands Directly
Redis-py provides the `execute_command` method to directly invoke Redis operations.  This functionality can be used to access any modules that may not have a supported interface in the redis-py client.  For example, you can use the `execute_command` to list all of the modules loaded into a Redis server:

    r.execute_command('MODULE', 'LIST')

