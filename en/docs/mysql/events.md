---
title: "Events"
slug: "events"
draft: false
images: []
weight: 9917
type: docs
toc: true
---

## Create an Event
Mysql has its EVENT functionality for avoiding complicated cron interactions when much of what you are scheduling is SQL related, and less file related. See the Manual page [here][1]. Think of Events as Stored Procedures that are scheduled to run on recurring intervals.

To save time in debugging Event-related problems, keep in mind that the global event handler must be turned on to process events.

    SHOW VARIABLES WHERE variable_name='event_scheduler';
    +-----------------+-------+
    | Variable_name   | Value |
    +-----------------+-------+
    | event_scheduler | OFF   |
    +-----------------+-------+

With it OFF, nothing will trigger. So turn it on:

    SET GLOBAL event_scheduler = ON;

Schema for testing
----

    create table theMessages
    (   id INT AUTO_INCREMENT PRIMARY KEY,
        userId INT NOT NULL,
        message VARCHAR(255) NOT NULL,
        updateDt DATETIME NOT NULL,
        KEY(updateDt)
    );
     
    INSERT theMessages(userId,message,updateDt) VALUES (1,'message 123','2015-08-24 11:10:09');
    INSERT theMessages(userId,message,updateDt) VALUES (7,'message 124','2015-08-29');
    INSERT theMessages(userId,message,updateDt) VALUES (1,'message 125','2015-09-03 12:00:00');
    INSERT theMessages(userId,message,updateDt) VALUES (1,'message 126','2015-09-03 14:00:00');

The above inserts are provided to show a starting point. Note that the 2 events created below will clean out rows.

Create 2 events, 1st runs daily, 2nd runs every 10 minutes
---

Ignore what they are actually doing (playing against one another). The point is on the INTERVAL and scheduling.

    DROP EVENT IF EXISTS `delete7DayOldMessages`;
    DELIMITER $$
    CREATE EVENT `delete7DayOldMessages`
      ON SCHEDULE EVERY 1 DAY STARTS '2015-09-01 00:00:00'
      ON COMPLETION PRESERVE
    DO BEGIN
       DELETE FROM theMessages 
       WHERE datediff(now(),updateDt)>6; -- not terribly exact, yesterday but <24hrs is still 1 day
       
      -- Other code here
    
    END$$
    DELIMITER ;

...

    DROP EVENT IF EXISTS `Every_10_Minutes_Cleanup`;
    DELIMITER $$
    CREATE EVENT `Every_10_Minutes_Cleanup`
      ON SCHEDULE EVERY 10 MINUTE STARTS '2015-09-01 00:00:00'
      ON COMPLETION PRESERVE
    DO BEGIN
       DELETE FROM theMessages 
       WHERE TIMESTAMPDIFF(HOUR, updateDt, now())>168; -- messages over 1 week old (168 hours)
    
       -- Other code here
    END$$
    DELIMITER ;

Show event statuses (different approaches)
-----

    SHOW EVENTS FROM my_db_name; -- List all events by schema name (db name)
    SHOW EVENTS; 
    SHOW EVENTS\G; -- <--------- I like this one from mysql> prompt

    *************************** 1. row ***************************
                      Db: my_db_name
                    Name: delete7DayOldMessages
                 Definer: root@localhost
               Time zone: SYSTEM
                    Type: RECURRING
              Execute at: NULL
          Interval value: 1
          Interval field: DAY
                  Starts: 2015-09-01 00:00:00
                    Ends: NULL
                  Status: ENABLED
              Originator: 1
    character_set_client: utf8
    collation_connection: utf8_general_ci
      Database Collation: utf8_general_ci
    *************************** 2. row ***************************
                      Db: my_db_name
                    Name: Every_10_Minutes_Cleanup
                 Definer: root@localhost
               Time zone: SYSTEM
                    Type: RECURRING
              Execute at: NULL
          Interval value: 10
          Interval field: MINUTE
                  Starts: 2015-09-01 00:00:00
                    Ends: NULL
                  Status: ENABLED
              Originator: 1
    character_set_client: utf8
    collation_connection: utf8_general_ci
      Database Collation: utf8_general_ci
    2 rows in set (0.06 sec)

Random stuff to consider
-----

`DROP EVENT someEventName;` -- Deletes the event and its code

`ON COMPLETION PRESERVE` -- When the event is done processing, retain it. Otherwise, it is deleted.

Events are like triggers. They are not called by a user's program. Rather, they are scheduled. As such, they succeed or fail silently.

The link to the Manual Page shows quite a bit of flexibilty with interval choices, shown below:

> interval:
> 
>     quantity {YEAR | QUARTER | MONTH | DAY | HOUR | MINUTE |
>               WEEK | SECOND | YEAR_MONTH | DAY_HOUR | DAY_MINUTE |
>               DAY_SECOND | HOUR_MINUTE | HOUR_SECOND | MINUTE_SECOND}

Events are powerful mechanisms that handle recurring and scheduled tasks for your system. They may contain as many statements, DDL and DML routines, and complicated joins as you may reasonably wish. Please see the MySQL Manual Page entitled [Restrictions on Stored Programs][2].

[1]: https://dev.mysql.com/doc/refman/5.7/en/create-event.html
[2]: http://dev.mysql.com/doc/refman/5.7/en/stored-program-restrictions.html


