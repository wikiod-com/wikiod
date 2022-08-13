---
title: "Redis Persistence Storage"
slug: "redis-persistence-storage"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Redis supports two main modes of persistence: RDB and AOF.  The RDB mode of persistence takes a snapshot of your database at a point in time.  In the RDB mode, Redis forks off a process to persist the database to disk.  AOF logs every operation executed against the server into a replay log that can be processed at startup to restore the state of the database.

## Disable all persistence storage in Redis
There are two kinds of persistent storage modes in Redis: AOF and RDB.  To temporarily disable RDB execute the following commands on the Redis command line:
```
config set save ""
```
to temporarily disable AOF execute the following from the Redis command line:
```
config set appendonly no
```

The changes will persist until the server is restarted, then the server will revert back to whatever modes are configured in the server's redis.conf file.

The ```CONFIG REWRITE``` command can be used to modify the redis.conf file to reflect any dynamic changes to the configuration.

## Get persistence storage status
The following code will get the current configuration for the persistent storage state.  These values can be modified dynamically, so they may differ from the configuration in redis.conf:

```
# get
config get appendonly
config get save
```

