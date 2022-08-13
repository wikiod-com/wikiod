---
title: "EXTENSION dblink and postgres_fdw"
slug: "extension-dblink-and-postgres_fdw"
draft: false
images: []
weight: 9843
type: docs
toc: true
---

## Syntax
 - dblink ('dbname = name_db_distance port = PortOfDB host = HostOfDB user = usernameDB 
password = passwordDB', 'MY QUESRY')

 - dbname = name of the database
 - port = Port Of the database  
 - host = Host Of the database 
 - user = username of the database 
 - password = password of the database', 
 - MY QUESRY = this can be any operation i want to do SELECT, INSERT, ...

## Extention FDW
FDW is an implimentation of dblink it is more helpful, so to use it:

1-Create an extention:

    CREATE EXTENSION postgres_fdw;

2-Create SERVER:

    CREATE SERVER name_srv FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host 'hostname', 
    dbname 'bd_name', port '5432');

3-Create user mapping for postgres server

    CREATE USER MAPPING FOR postgres SERVER name_srv OPTIONS(user 'postgres', password 'password');

4-Create foreign table:

    CREATE FOREIGN TABLE table_foreign (id INTEGER, code character varying) 
    SERVER name_srv OPTIONS(schema_name 'schema', table_name 'table');

5-use this foreign table like it is in your database:

    SELECT * FROM table_foreign;

## Foreign Data Wrapper
To access complete schema of server db instead of single table. Follow below steps:

1. Create EXTENSION :
<pre>
    CREATE EXTENSION postgres_fdw;
</pre>
2. Create SERVER :
<pre>
    CREATE SERVER server_name FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host 'host_ip', 
    dbname 'db_name', port 'port_number');
</pre>
3. Create USER MAPPING:
<pre>
    CREATE USER MAPPING FOR CURRENT_USER
    SERVER server_name
    OPTIONS (user 'user_name', password 'password');
</pre>
4. Create new schema to access schema of server DB: 
<pre>
   CREATE SCHEMA schema_name;
</pre>
5. Import server schema: 
<pre>
     IMPORT FOREIGN SCHEMA schema_name_to_import_from_remote_db
     FROM SERVER server_name
     INTO schema_name;
</pre>
6. Access any table of server schema:
<pre>
    SELECT * FROM schema_name.table_name; 
</pre>

This can be used to access multiple schema of remote DB.

## Extention  dblink
dblink EXTENSION is a technique to connect another database and make operation of this database so to do that you need:

1-Create a dblink extention:

    CREATE EXTENSION dblink;

2-Make your operation:

For exemple Select some attribute from another table in another database:

    SELECT * FROM 
    dblink ('dbname = bd_distance port = 5432 host = 10.6.6.6 user = username 
    password = passw@rd', 'SELECT id, code FROM schema.table') 
    AS newTable(id INTEGER, code character varying);



