---
title: "Upgrading MongoDB version"
slug: "upgrading-mongodb-version"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

How to update the version of MongoDB on your machine on different platforms and versions.

If you have an older version of MongoDB, you must upgrade the whole path to the newest version. For example, if you are running version 3.0 and want to get version 3.4, you must upgrade 3.0->3.2->3.4.

## Upgrading to 3.4 on Ubuntu 16.04 using apt
You must have 3.2 to be able to upgrade to 3.4. This example assumes you are using `apt`.
0. `sudo service mongod stop`
1. `sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6`
2. `echo "deb [ arch=amd64,arm64 ] http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.4.list`
3. `sudo apt-get update`
4. `sudo apt-get upgrade`
5. `sudo service mongod start`

Ensure the new version is running with `mongo`. The shell will print out the MongoDB server version that should be 3.4 now.

