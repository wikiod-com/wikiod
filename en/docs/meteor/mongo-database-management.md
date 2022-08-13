---
title: "Mongo Database Management"
slug: "mongo-database-management"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

If you're not opposed to running a Ruby utility, Genghis is a classic: http://genghisapp.com/

But for scalable production use, get yourself to MongoHQ.  
http://www.mongohq.com/

Also, the Mongo Monitoring Service, from 10Gen, the makers of Mongo:  
https://mms.mongodb.com/

[MongoClient](http://www.mongoclient.com) is written in Meteor, Completely Free, Open Source And Cross-Platform.

[RoboMongo](https://robomongo.org/) Native cross-platform MongoDB management tool

## Analyzing An Inherited Database
There's two great utilities for black-box analysis of databases. First is variety.js, which will give you a high-level overview. The second is schema.js, which will let you dig into the collections for more detail on the individual fields.  When inheriting a production Mongo database, these two utilities can help you make sense of what's going on and how the collections and documents are structured. 

[variety.js](https://github.com/variety/variety)  

```
mongo test --eval "var collection = 'users'" variety.js
```

[schema.js](http://skratchdot.com/projects/mongodb-schema/)  

```
mongo --shell schema.js 
```



## Connect To A Database on *.meteorapp.com
The --url flag can be tricky to use. There is a 60 second window to authenticate, and then the username/password randomly resets. So be sure to have robomongo open and ready to configure a new connection when you run the command.

```
# get the MONGO_URL string for your app  
meteor mongo --url $METEOR_APP_URL
```

## Download a Database from *.meteor.com
Same thing as before, but you have to copy the info into the mongodump command. You have to run the following commands rediculously fast, and it requires hand/eye coordination. Be warned! This is a rediculously hacky! But fun! Think of it as a video game! :D

```
# get the MONGO_URL string for your app  
meteor mongo --url $METEOR_APP_URL

# then quickly copy all the info into the following command
mongodump -u username -p password --port 27017 --db meteor_app_url_com --host production-db-b1.meteor.io
```


## Export Data from local Meteor development instance?
This command will create a /dump directory, and store each collection in a separate BSON blob file. This is the best way to backup or transfer databases between systems.

```
mongodump --db meteor
```

## Restore Data from a Dumpfile
The analog to the ``meteordump`` command is ``meteorrestore``. You can do a partial import by selecting the specific collection to import. Particularly useful after running a drop command.

```
# make sure your app is running
meteor

# then import your data
mongorestore --port 3001 --db meteor /path/to/dump

# a partial import after running > db.comments.drop()
mongorestore --port 3001 --db meteor /path/to/dump -c comments.bson
```

## Export a Collection to JSON
Run meteor, open another terminal window, and run the following command. 
```
mongoexport --db meteor --collection foo --port 3001 --out foo.json
```

## Import a JSON File into Meteor
Importing into a default Meteor instance is fairly easy. Note that you can add a --jsonArray option if your json file is exported as an array from another system.

```
mongoimport --db meteor --port 3001 --collection foo --file foo.json
```

## Copying Data Between Staging and Local Databases
Mongo supports database-to-database copying, which is useful if you have large databases on a staging database that you want to copy into a local development instance.

```sh
// run mongod so we can create a staging database
// note that this is a separate instance from the meteor mongo and minimongo instances
mongod

// import the json data into a staging database
// jsonArray is a useful command, particularly if you're migrating from SQL
mongoimport -d staging -c assets < data.json --jsonArray

// navigate to your application
cd myappdir

// run meteor and initiate it's database
meteor

// connect to the meteor mongodb
meteor mongo --port 3002

// copy collections from staging database into meteor database
db.copyDatabase('staging', 'meteor', 'localhost');
```

## Compact a Mongo Database on an Ubuntu Box
Preallocation. Mongo sets aside disk-space in empty containers, so when the time comes to write something to disk, it doesn't have to shuffle bits out of the way first. It does so by a doubling algorithm, always doubling the amount of disk space preallocated until it reaches 2GB; and then each prealloc file from thereon is 2GB. Once data is preallocated, it doesn't unallocate unless you specifically tell it to. So observable MongoDB space usage tends to go up automatically, but not down.

Some research on the Mongo preallocation...  
[reducing-mongodb-database-file-size](http://stackoverflow.com/questions/2966687/reducing-mongodb-database-file-size)  
[mongo-prealloc-files-taking-up-room](http://stackoverflow.com/questions/9473850/mongo-prealloc-files-taking-up-room) 

```
// compact the database from within the Mongo shell
db.runCommand( { compact : 'mycollectionname' } )

// repair the database from the command line
mongod --config /usr/local/etc/mongod.conf --repair --repairpath /Volumes/X/mongo_repair --nojournal

// or dump and re-import from the command line
mongodump -d databasename
echo 'db.dropDatabase()' | mongo databasename
mongorestore dump/databasename
``` 

## Reset a Replica Set
Delete the local database files. Just exit the Mongo shell, navigate to the /dbpath (wherever you set it up), and delete the files within that directory.

## Connect Remotely to a Mongo Instance on *.meteor.com
Did you know about the ``--url`` flag? Very handy.

```
meteor mongo --url YOURSITE.meteor.com
```

## Accessing Mongo Log Files on a Local Meteor Instance
They're not easily accessible. If you run the 'meteor bundle' command, you can generate a tar.gz file, and then run your app manually. Doing that, you should be able to access the mongo logs... probably in the .meteor/db directory.
If you really need to access mongodb log files, set up a regular mongodb instance, and then connect Meteor to an external mongo instance, by setting the MONGO_URL environment variable:

```
MONGO_URL='mongodb://user:password@host:port/databasename'
```

Once that's done, you should be able to access logs in the usual places...

```
/var/log/mongodb/server1.log
```

## Rotate Log Files on an Ubuntu Box
Gotta rotate those log files, or they'll eventually eat up all of your disk space. Start with some research...  
[mongodb-log-file-growth](http://stackoverflow.com/questions/5004626/mongodb-log-file-growth)  
[rotate-log-files](http://docs.mongodb.org/manual/tutorial/rotate-log-files/)  

Log files can be viewed with the following command...

```
ls /var/log/mongodb/
```

But to set up log-file rotation, you'll need to do the following...

```
// put the following in the /etc/logrotate.d/mongod file
/var/log/mongo/*.log {
    daily
    rotate 30
    compress
    dateext
    missingok
    notifempty
    sharedscripts
    copytruncate
    postrotate
        /bin/kill -SIGUSR1 `cat /var/lib/mongo/mongod.lock 2> /dev/null` 2> /dev/null || true
    endscript
}

// to manually initiate a log file rotation, run from the Mongo shell
use admin
db.runCommand( { logRotate : 1 } )
```

