---
title: "Getting database information"
slug: "getting-database-information"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## List all databases
    show dbs

or

    db.adminCommand('listDatabases')

or

    db.getMongo().getDBNames()

## List all collections in database
    show collections

or

    show tables

or

    db.getCollectionNames()

