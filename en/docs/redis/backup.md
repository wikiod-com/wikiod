---
title: "Backup"
slug: "backup"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Backing up a remote Redis instance can be achieved with replication. This is useful if you want to take a snapshot of a dataset prior to upgrading, deleting or changing a Redis database.

## Backup of a remote Redis instance to a local instance
On the machine where you'd like to make the backup, jump to the Redis CLI:

    redis-cli

## Password?

If your master Redis DB (the one you want to replicate) has a password:

    config set masterauth <password>


## Start replication

Run the following to begin replication:


    SLAVEOF <host> <port>


To check the replication is underway run:


    INFO replication

And you should see output like this:


    # Replication
    role:slave
    master_host:some-host.compute-1.amazonaws.com
    master_port:6519
    master_link_status:up
    master_last_io_seconds_ago:3
    master_sync_in_progress:0
    slave_repl_offset:35492914
    slave_priority:100
    slave_read_only:1
    connected_slaves:0
    master_repl_offset:0
    repl_backlog_active:0
    repl_backlog_size:1048576
    repl_backlog_first_byte_offset:0
    repl_backlog_histlen:0

Note the `master_link_status` should be `up`.

## Checking sync progress

When the sync is complete, the `INFO replication` should show:

    master_sync_in_progress:0


To check the dataset has been synced you could compare the size of the database:

    DBSIZE

## Saving a data dump to disk

To save the DB to disk asynchronously:

    BGSAVE
    CONFIG GET dir

Then you should find a `dump.rdb` file in the directory listed by the config command. 

## Halting replication

You can stop replication with:

    SLAVEOF NO ONE


----------

Reference: [Redis replication guide][1]


  [1]: https://redis.io/topics/replication

