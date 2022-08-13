---
title: "Replica Sets and Sharding"
slug: "replica-sets-and-sharding"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

For those not familiar, a Replica Set is defined as a redundant configuration of three servers. A Sharded Database is defined as a horizintally scalled database, where each Shard is defined as a Replica Set. Therefore, a sharded Mongo cluster involves a minimum of 11 servers for a 2 shard cluster, and increases by three servers for each additional shard. So, a sharded cluster always has 11, 14, 17, 20, 23, etc server instances. That is, there's 2 shards of 3 servers each, 3 more config controllers, and 2 routers. 11 servers total for a 2 shard cluster.

## Replica Set Quickstart
Build yourself **three** servers using whatever physical or virtual hardware you wish. (This tutorial assumes you're using Ubuntu as your operating system.) Then repeat the following instructions three times... once for each server.
<!-- language: lang-bash -->
```
# add the names of each server to the host file of each server
sudo nano /etc/hosts
  10.123.10.101 mongo-a
  10.123.10.102 mongo-b
  10.123.10.103 mongo-c

# install mongodb on the server
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | sudo tee /etc/apt/sources.list.d/mongodb.list
sudo apt-get update
sudo apt-get install mongodb-10gen


# create the /data/ directories
sudo mkdir /data
sudo mkdir /data/logs
sudo mkdir /data/db

# make sure the mongodb user and group have access to our custom directories
sudo chown -R mongodb:mongodb /data

# edit the mongo upstart file in /etc/init/mongodb.conf
sudo nano /etc/init/mongodb.conf
  start on started mountall
  stop on shutdown
  respawn
  respawn limit 99 5
  setuid mongodb
  setgid mongodb
  script
    exec /usr/bin/mongod --config /etc/mongodb.conf >> /data/logs/mongo-a.log 2>&1
  end script


# edit mongodb configuration file
sudo nano /etc/mongodb.conf
    dbpath=/data/db
    logpath=/data/logs/mongod.log
    logappend=true
    port=27017
    noauth=true
    replSet=meteor
    fork=true

# add a mongo log-rotation file
sudo nano /etc/logrotate.d/mongod
  /data/logs/*.log {
    daily
    rotate 30
    compress
    dateext
    missingok
    notifempty
    sharedscripts
    copytruncate
    postrotate
        /bin/kill -SIGUSR1 `cat /data/db/mongod.lock 2> /dev/null` 2> /dev/null || true
    endscript
  }

# make sure mongod service is started and running
sudo service mongodb start
sudo reboot
```

## Replica Set Configuration
Then go into the mongo shell and initiate the replica set, like so:

<!-- language: lang-bash-->
```
meteor mongo

  > rs.initiate()
  PRIMARY> rs.add("mongo-a")
  PRIMARY> rs.add("mongo-b")
  PRIMARY> rs.add("mongo-c")
  PRIMARY> rs.setReadPref('secondaryPreferred')
```

