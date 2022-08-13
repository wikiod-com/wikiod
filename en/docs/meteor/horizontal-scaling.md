---
title: "Horizontal Scaling"
slug: "horizontal-scaling"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Deploying an Application with Separated Database (MONGO_URL)
You'll need to separate out your application layer from your database layer, and that means specifying the MONGO_URL.  Which means running your app through the bundle command, uncompressing it, setting environment variables, and then launching the project as a node app. Here's how...

```
#make sure you're running the node v0.10.21 or later
npm cache clean -f
npm install -g n
sudo n 0.10.21

# bundle the app
mkdir myapp
cd myapp 
git clone http://github.com/myaccount/myapp
meteor bundle --directory ../deployPath
cd ../deployPath

# make sure fibers is installed, as per the README
export MONGO_URL='mongodb://127.0.0.1:27017/mydatabase'
export PORT='3000'
export ROOT_URL='http://myapp.com'

# run the site
node main.js
```

## Replica Set Configuration
Then go into the mongo shell and initiate the replica set, like so:

```
mongo

> rs.initiate()
PRIMARY> rs.add("mongo-a")
PRIMARY> rs.add("mongo-b")
PRIMARY> rs.add("mongo-c")
PRIMARY> rs.setReadPref('secondaryPreferred')
```

## Configuring a Replica Set to Use Oplogging
The replica set will need an oplog user to access the database.
```
mongo

PRIMARY> use admin
PRIMARY> db.addUser({user:"oplogger",pwd:"YOUR_PASSWORD",roles:[],otherDBRoles:{local:["read"]}});
PRIMARY> show users

```

## Oplog Upstart Script
Your upstart script will need to be modified to use multiple IP addresses of the replica set.
```
start on started mountall
stop on shutdown

respawn
respawn limit 99 5

script
    # our example assumes you're using a replica set and/or oplog integreation
    export MONGO_URL='mongodb://mongo-a:27017,mongo-b:27017,mongo-c:27017/meteor'

    # here we configure our OPLOG URL
    export MONGO_OPLOG_URL='mongodb://oplogger:YOUR_PASSWORD@mongo-a:27017,mongo-b:27017,mongo-c:27017/local?authSource=admin'

    # root_url and port are the other two important environment variables to set
    export ROOT_URL='http://myapp.mydomain.com'
    export PORT='80'

    exec /usr/local/bin/node /var/www/production/main.js >> /var/log/node.log 2>&1
end script
```

## Sharding
[Oplog Tailing on Sharded Mongo](https://groups.google.com/forum/#!topic/meteor-core/G_Hgca1xi_8)  

