---
title: "Getting started with db2"
slug: "getting-started-with-db2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
There different flavors of DB2. One of them is LUW: Linux, UNIX and Windows.

DB2 LUW in Linux / UNIX can be installed with or without root. When installed with root, you can create different instances associating them to different users.

When installing DB2 LUW without root privileges, you can install DB2 in your home directory and your user will be automatically the only instance this installation can have. The instance will not be started automatically each time the machine boots at least you configure that.

Once you have downloaded the binaries (from Fixpack Central, Passport Advantage, or a CD), you can extract the files. There will be a directory that describes the DB2 edition you a using (Expc, server_t, etc.), and in that directory you will find `db2setup` and `db2_install`. One of them is for graphic installation, the other is for text installation via response file.

