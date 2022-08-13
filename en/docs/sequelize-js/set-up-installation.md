---
title: "Set Up  Installation"
slug: "set-up--installation"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Install necessary modules
In order to start using Sequelize, you must first install the Sequelize package using the Node Package Manager (`npm`).

**Install sequelize module**

Add `--save` option to store module in package.json

    npm install sequelize

Next, depending on which database system you wish to use with Sequelize, install the appropriate package. Sequelize currently supports MySQL, PostgreSQL, SQLite and Microsoft SQL Server.

**Install MySQL for MySQL database**

    npm install mysql

**Install SQLite for SQLite database**

    npm install sqlite3

**Install PostgreSQL (and hstore) for PostgreSQL database**

    npm install pg pg-hstore

**Install MSSQL (and `tedious` driver) for Microsoft SQL Server**

    npm install tedious mssql


This information can also be found in Sequelize's [Documentation](http://docs.sequelizejs.com/en/latest/docs/getting-started/).

## For sqlite package in nw.js
 1.  `npm install sqlite3 --build-from-source --runtime=node-webkit --target_arch=ia32 --target=` target is important. ex:0.16.1
 2. `npm rebuild`
 3. Create a folder for sqlite db. 
 4. Remember sequalize.sync();

