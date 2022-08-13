---
title: "PostgreSQL Database Adapter for Sails"
slug: "postgresql-database-adapter-for-sails"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Install
You can install the postgreSQL adapter via NPM.

    npm install sails-postgresql


## Configuration
You can configure the database settings in `config/connections.js`.

Here's an example:

```
postgresql: {
  database: 'databaseName',
  host: 'localhost',
  user: 'root',
  password: '',
  port: 5432,
  poolSize: 10,
  ssl: false
};
```

Alternatively, you can supply the connection information in URL format:

```
postgresql: {
  url: 'postgres://username:password@hostname:port/database',
  ssl: false
};
```
     
     

