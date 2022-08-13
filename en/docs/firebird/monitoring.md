---
title: "Monitoring"
slug: "monitoring"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

With Firebird 2.1 and databases with [ODS 11.1 (and higher)][1] Firebird introduces the ability to monitor server-side activity happening inside a particular database.

Complete database monitoring is available to SYSDBA and the database owner. Regular users are restricted to the information about their own attachments only—other attachments are invisible to them.

Available monitoring tables since Firebird 2.1 and ODS 11.1:

 - MON$DATABASE (connected database)
 - MON$ATTACHMENTS (connected attachments)
 - MON$TRANSACTIONS (started transactions)
 - MON$STATEMENTS (prepared statements)
 - MON$CALL_STACK (call stack of active PSQL requests)
 - MON$IO_STATS (I/O statistics)
 - MON$RECORD_STATS (record-level statistics)

Reference: [Firebird 2.1 Release Notes][2]

With Firebird 2.5 and databases with [ODS 11.2 (and higher)][1] Firebird adds the following new monitor tables

 - MON$MEMORY_USAGE (current memory usage)
 - MON$CONTEXT_VARIABLES (known context variables)

Reference: [Firebird 2.5 Release Notes][3]


  [1]: https://www.wikiod.com/firebird/getting-started-with-firebird#What is ODS version and how to retrieve it?
  [2]: http://www.firebirdsql.org/file/documentation/release_notes/html/rlsnotes217.html#rnfb210-mon
  [3]: http://firebirdsql.org/rlsnotesh/rlsnotes25.html#rnfb25-mon

## Get information about attachments on the connected database
Information about the database connections
   

    SELECT
           a.mon$attachment_id as Attachment_ID,
           a.mon$server_pid as Server_PID, 
           case a.mon$state 
              when 1 then 'active'
              when 0 then 'idle'
           end as State, 
           a.mon$attachment_name as Database_Name, 
           a.mon$user as User_Name, 
           a.mon$role as Role_Name, 
           a.mon$remote_protocol as Remote_Protocol, 
           a.mon$remote_address as  Remote_Address, 
           a.mon$remote_pid as Remote_PID, 
           cs.rdb$character_set_name as Connection_Character_Set, 
           a.mon$timestamp as Established_At,
           case a.mon$garbage_collection 
              when 1 then 'allowed'
              when 0 then 'not allowed'
           end as Garbage_Collection, 
           a.mon$remote_process as Remote_Process, 
           a.mon$stat_id as Statistics_ID
        FROM
           mon$attachments a, rdb$character_sets cs
        where 
           (a.mon$character_set_id = cs.rdb$character_set_id)

Results:

[![enter image description here][1]][1]

**More specific examples**

Information about the connected clients.

    SELECT
       a.mon$remote_protocol as Remote_Protocol,
       a.mon$remote_address as  Remote_Address,
       a.mon$remote_pid as Remote_PID,
       a.mon$timestamp as Established_At,
       a.mon$remote_process as Remote_Process
    FROM
       mon$attachments a

Retrieve PIDs of all server processes loading CPU at the moment (interesting with a Classic Server Architecture)

    SELECT
       MON$SERVER_PID
    FROM
       MON$ATTACHMENTS
    WHERE
       MON$STATE = 1


Retrieve information about the connected users, workstations and the client applications

    SELECT
       mon$attachment_name as Database_Name,
       mon$user as User_Name,
       mon$role as Role_Name,
       mon$remote_process as Client_Application,
       mon$remote_address as Client_IP,
       mon$remote_pid as Client_Application_PID
    FROM
       mon$attachments

----------

Reference:
----------
- [Firebird 2.1 Release Notes][2]
- [Firebird 2.5 Language Reference][3]


  [1]: http://i.stack.imgur.com/guVnQ.png
  [2]: http://www.firebirdsql.org/file/documentation/release_notes/html/rlsnotes217.html#rnfb210-mon
  [3]: https://ib-aid.com/download/docs/firebird-language-reference-2.5/fblangref-appx05-monattach.html


