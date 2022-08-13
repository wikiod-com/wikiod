---
title: "Triggers and Trigger Functions"
slug: "triggers-and-trigger-functions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

The trigger will be associated with the specified table or view and will execute the specified function function_name when certain events occur.

Please use below link for complete overview of:

- **Triggers**: https://www.postgresql.org/docs/current/static/sql-createtrigger.html
- **Trigger Functions**: https://www.postgresql.org/docs/current/static/plpgsql-trigger.html


## Basic PL/pgSQL Trigger Function
This is a simple trigger function. 

    CREATE OR REPLACE FUNCTION my_simple_trigger_function()
    RETURNS trigger AS
    $BODY$

    BEGIN
        -- TG_TABLE_NAME :name of the table that caused the trigger invocation
    IF (TG_TABLE_NAME = 'users') THEN

        --TG_OP : operation the trigger was fired
      IF (TG_OP = 'INSERT') THEN 
        --NEW.id is holding the new database row value (in here id is the id column in users table)
        --NEW will return null for DELETE operations
        INSERT INTO log_table (date_and_time, description) VALUES (now(), 'New user inserted. User ID: '|| NEW.id);        
        RETURN NEW;        

      ELSIF (TG_OP = 'DELETE') THEN    
        --OLD.id is holding the old database row value (in here id is the id column in users table)
        --OLD will return null for INSERT operations
        INSERT INTO log_table (date_and_time, description) VALUES (now(), 'User deleted.. User ID: ' || OLD.id);
        RETURN OLD;        
        
      END IF;

    RETURN null;
    END IF;

    END;
    $BODY$
    LANGUAGE plpgsql VOLATILE
    COST 100;


Adding this trigger function to the `users` table

    CREATE TRIGGER my_trigger
    AFTER INSERT OR DELETE
    ON users
    FOR EACH ROW
    EXECUTE PROCEDURE my_simple_trigger_function();

## Type of triggers
# Trigger can be specified to fire:

- <code>BEFORE</code> the operation is attempted on a row - insert, update or delete;
- <code>AFTER</code> the operation has completed - insert, update or delete;
- <code>INSTEAD OF</code> the operation in the case of inserts, updates or deletes on a view.

# Trigger that is marked:
- <code>FOR EACH ROW</code> is called once for every row that the operation modifies;
- <code>FOR EACH STATEMENT</code> is called onde for any given operation.

# Preparing to execute examples
    CREATE TABLE company (
        id          SERIAL PRIMARY KEY NOT NULL,
        name        TEXT NOT NULL,
        created_at  TIMESTAMP,
        modified_at TIMESTAMP DEFAULT NOW()
    )

    CREATE TABLE log (
        id          SERIAL PRIMARY KEY NOT NULL,
        table_name  TEXT NOT NULL,
        table_id    TEXT NOT NULL,
        description TEXT NOT NULL,
        created_at  TIMESTAMP DEFAULT NOW()
    )

# Single insert trigger

## Step 1: create your function

    CREATE OR REPLACE FUNCTION add_created_at_function()
      RETURNS trigger AS $BODY$
    BEGIN
      NEW.created_at := NOW();
      RETURN NEW;
    END $BODY$
    LANGUAGE plpgsql;

## Step 2: create your trigger

    CREATE TRIGGER add_created_at_trigger
    BEFORE INSERT
    ON company
    FOR EACH ROW
    EXECUTE PROCEDURE add_created_at_function();

## Step 3: test it

    INSERT INTO company (name) VALUES ('My company');
    SELECT * FROM company;

# Trigger for multiple purpose

## Step 1: create your function

    CREATE OR REPLACE FUNCTION add_log_function()
      RETURNS trigger AS $BODY$
    DECLARE
      vDescription TEXT;
      vId INT;
      vReturn RECORD;
    BEGIN
        vDescription := TG_TABLE_NAME || ' ';
         IF (TG_OP = 'INSERT') THEN
            vId := NEW.id;
            vDescription := vDescription || 'added. Id: ' || vId;
            vReturn := NEW;
        ELSIF (TG_OP = 'UPDATE') THEN
            vId := NEW.id;
            vDescription := vDescription || 'updated. Id: ' || vId;
            vReturn := NEW;
        ELSIF (TG_OP = 'DELETE') THEN
            vId := OLD.id;
            vDescription := vDescription || 'deleted. Id: ' || vId;
            vReturn := OLD;
        END IF;
    
        RAISE NOTICE 'TRIGER called on % - Log: %', TG_TABLE_NAME, vDescription;
    
        INSERT INTO log 
            (table_name, table_id, description, created_at) 
            VALUES
            (TG_TABLE_NAME, vId, vDescription, NOW());
    
        RETURN vReturn;
    END $BODY$
      LANGUAGE plpgsql;

## Step 2: create your trigger

    CREATE TRIGGER add_log_trigger
    AFTER INSERT OR UPDATE OR DELETE
    ON company
    FOR EACH ROW
    EXECUTE PROCEDURE add_log_function();

## Step 3: test it

    INSERT INTO company (name) VALUES ('Company 1');
    INSERT INTO company (name) VALUES ('Company 2');
    INSERT INTO company (name) VALUES ('Company 3');
    UPDATE company SET name='Company new 2' WHERE name='Company 2';
    DELETE FROM company WHERE name='Company 1';
    SELECT * FROM log;

