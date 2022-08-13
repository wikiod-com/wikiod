---
title: "Isolation levels and locking"
slug: "isolation-levels-and-locking"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

I found this link - it's useful as a reference:
["Isolation Levels"][1]


  [1]: https://www.simple-talk.com/sql/t-sql-programming/questions-about-t-sql-transaction-isolation-levels-you-were-too-shy-to-ask/ 

## Examples of setting the isolation level
Example of setting the isolation level:

    SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
    SELECT * FROM Products WHERE ProductId=1;
    SET TRANSACTION ISOLATION LEVEL REPEATABLE READ; --return to the default one

1. `READ UNCOMMITTED` - means that a query in the current transaction can't access the modified data from another transaction that is not yet committed - no dirty reads!
BUT, nonrepeatable reads and phantom reads are possible, because data can still be modified by other transactions.

2. `REPEATABLE READ` - means that a query in the the current transaction can't access the modified data from another transaction that is not yet committed - no dirty reads!
No other transactions can modify data being read by the current transaction until it is completed, which eliminates NONREPEATABLE reads. BUT, if another transaction inserts NEW ROWS and the query is executed more then once, phantom rows can appear starting the second read (if it matches the where statement of the query).

3. `SNAPSHOT` - only able to return data that exists at the beginning of the query. Ensures consistency of the data. It prevents dirty reads, nonrepeatable reads and phantom reads.
To use that - DB configurationis required:


    ALTER DATABASE DBTestName SET ALLOW_SNAPSHOT_ISOLATION ON;GO;
    SET TRANSACTION ISOLATION LEVEL SNAPSHOT;


4. `READ COMMITTED` - default isolation of the SQL server. It prevents reading the data that is changed by another transaction until committed. It uses shared locking and row versioning on the tables which prevents dirty reads. It depends on DB configuration READ_COMMITTED_SNAPSHOT - if enabled - row versioning is used.
to enable - use this:


    ALTER DATABASE DBTestName SET ALLOW_SNAPSHOT_ISOLATION ON;GO;
    SET TRANSACTION ISOLATION LEVEL READ COMMITTED; --return to the default one

5. `SERIALIZABLE` - uses physical locks that are acquired and held until end of the transaction, which prevents dirty reads, phantom reads, nonrepeatable reads. BUT, it impacts on the performance of the DataBase, because the concurrent transactions are serialized and are being executed one by one.


    SET TRANSACTION ISOLATION LEVEL SERIALIZABLE ;

