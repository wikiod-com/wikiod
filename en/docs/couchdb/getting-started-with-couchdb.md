---
title: "Getting started with couchdb"
slug: "getting-started-with-couchdb"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
# Ubuntu

On recent Ubuntu versions, you can install an up-to-date version of CouchDB with `sudo apt-get install couchdb`. For older versions, such as Ubuntu 14.04, you should run:
```
sudo add-apt-repository ppa:couchdb/stable -y
sudo apt-get update
sudo apt-get install couchdb -y
```
# Fedora
To install couchdb in fedora ryou can do `sudo dnf install couchdb`

# Mac OS X

To install CouchDB on Mac OS X, you can install the Mac app from the [CouchDB downloads section](http://couchdb.apache.org/#download).

# Windows

To install CouchDB on Windows, you can simply download the executable from [CouchDB downloads section](http://couchdb.apache.org/#download). 

# Hello World

By default, CouchDB listens on port 5984. Visiting [`http://127.0.0.1:5984`](http://127.0.0.1:5984) will yield a response that looks like this:

    {"couchdb":"Welcome","version":"1.6.1"}

CouchDB comes out-of-the-box with a GUI called *Futon*. You can find this interface at [`http://127.0.0.1:5984/_utils`](http://127.0.0.1:5984/_utils). Here, you can easily set up an administrator account and configure other important settings.

