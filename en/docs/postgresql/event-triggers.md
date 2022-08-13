---
title: "Event Triggers"
slug: "event-triggers"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

Event Triggers will be fired whenever event associated with them occurs in database.

Please use below link for complete overview of Event Triggers in PostgreSQL

https://www.postgresql.org/docs/9.3/static/event-trigger-definition.html

## Logging DDL Command Start Events

Event Type- 

 - `DDL_COMMAND_START`
 - `DDL_COMMAND_END`
 - SQL_DROP

This is example for creating an Event Trigger and logging `DDL_COMMAND_START` events.

    CREATE TABLE TAB_EVENT_LOGS(
      DATE_TIME TIMESTAMP,
      EVENT_NAME TEXT,
      REMARKS TEXT
    );
    
    CREATE OR REPLACE FUNCTION FN_LOG_EVENT()
      RETURNS EVENT_TRIGGER
      LANGUAGE SQL
      AS 
      $main$
        INSERT INTO TAB_EVENT_LOGS(DATE_TIME,EVENT_NAME,REMARKS)
          VALUES(NOW(),TG_TAG,'Event Logging');
      $main$;

    CREATE EVENT TRIGGER TRG_LOG_EVENT ON DDL_COMMAND_START
      EXECUTE PROCEDURE FN_LOG_EVENT();
    

