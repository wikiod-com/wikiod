---
title: "SELECT"
slug: "select"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## SELECT using WHERE
In this topic we will base on this table of users :

    CREATE TABLE sch_test.user_table
    (
      id serial NOT NULL,
      username character varying,
      pass character varying,
      first_name character varying(30),
      last_name character varying(30),
      CONSTRAINT user_table_pkey PRIMARY KEY (id)
    )

    +----+------------+-----------+----------+------+
    | id | first_name | last_name | username | pass |   
    +----+------------+-----------+----------+------+
    | 1  | hello      | world     | hello    | word |   
    +----+------------+-----------+----------+------+
    | 2  | root       | me        | root     | toor |   
    +----+------------+-----------+----------+------+


**Syntax**

Select every thing:

    SELECT * FROM schema_name.table_name WHERE <condition>;

Select some fields :

    SELECT field1, field2 FROM schema_name.table_name WHERE <condition>;

**Examples**
    
    -- SELECT every thing where id = 1
    SELECT * FROM schema_name.table_name WHERE id = 1;

    -- SELECT id where username = ? and pass = ?
    SELECT id FROM schema_name.table_name WHERE username = 'root' AND pass = 'toor';

    -- SELECT first_name where id not equal 1
    SELECT first_name FROM schema_name.table_name WHERE id != 1;

