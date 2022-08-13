---
title: "Backing up and Restoring Data"
slug: "backing-up-and-restoring-data"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Basic mongodump of local default mongod instance
    mongodump --db mydb --gzip --out "mydb.dump.$(date +%F_%R)"

This command will dump a bson gzipped archive of your local mongod 'mydb' database to the 'mydb.dump.{timestamp}' directory

## Basic mongorestore of local default mongod dump
    mongorestore --db mydb mydb.dump.2016-08-27_12:44/mydb --drop --gzip

This command will first drop your current 'mydb' database and then restore your gzipped bson dump from the 'mydb mydb.dump.2016-08-27_12:44/mydb' archive dump file.

