---
title: "UPDATE"
slug: "update"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Syntax
- UPDATE [ LOW_PRIORITY ] [ IGNORE ]
tableName
SET column1 = expression1,
    column2 = expression2,
    ...
[WHERE conditions];  //Simple single table update

- UPDATE [ LOW_PRIORITY ] [ IGNORE ]
tableName
SET column1 = expression1,
    column2 = expression2,
    ...
[WHERE conditions]
[ORDER BY expression [ ASC | DESC ]]
[LIMIT row_count];  //Update with order by and limit

- UPDATE [LOW_PRIORITY] [IGNORE] 
table1, table2, ...
    SET column1 = expression1,
        column2 = expression2,
        ...
    [WHERE conditions];  //Multiple Table update

## Basic Update
## Updating **one** row

    UPDATE customers SET email='luke_smith@email.com' WHERE id=1

This query updates the content of `email` in the `customers` table to the string `luke_smith@email.com` where the value of `id` is equal to 1. The old and new contents of the database table are illustrated below on the left and right respectively:

[![enter image description here][1]][1]

---

## Updating **all** rows

    UPDATE customers SET lastname='smith'

This query update the content of `lastname` for every entry  in the `customers` table. The old and new contents of the database table are illustrated below on the left and right respectively:

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/IeWcs.png
  [2]: http://i.stack.imgur.com/jUYMk.png

**Notice:** It is necessary to use conditional clauses (WHERE) in UPDATE query. If you do not use any conditional clause then all records of that table's attribute will be updated. In above example new value (Smith) of lastname in customers table set to all rows. 

## Update with Join Pattern
Consider a production table called `questions_mysql` and a table `iwtQuestions` (imported worktable) representing the last batch of imported CSV data from a [`LOAD DATA INFILE`][1]. The worktable is truncated before the import, the data is imported, and that process is not shown here.

Update our production data using a join to our imported worktable data.

    UPDATE questions_mysql q -- our real table for production 
    join iwtQuestions i -- imported worktable 
    ON i.qId = q.qId
    SET q.closeVotes = i.closeVotes,
    q.votes = i.votes, 
    q.answers = i.answers, 
    q.views = i.views;

Aliases `q` and `i` are used to abbreviate the table references. This eases development and readability.

`qId`, the Primary Key, represents the Stackoverflow question id. Four columns are updated for matching rows from the join.

[1]: http://dev.mysql.com/doc/refman/5.7/en/load-data.html

## UPDATE with ORDER BY and LIMIT
If the `ORDER BY` clause is specified in your update SQL statement, the rows are updated in the order that is specified.

If `LIMIT` clause is specified in your SQL statement, that places a limit on the number of rows that can be updated. There is no limit, if `LIMIT` clause not specified.

`ORDER BY` and `LIMIT` cannot be used for multi table update.

Syntax for the MySQL `UPDATE` with `ORDER BY` and `LIMIT` is,

    UPDATE [ LOW_PRIORITY ] [ IGNORE ]
    tableName
    SET column1 = expression1,
        column2 = expression2,
        ...
    [WHERE conditions]
    [ORDER BY expression [ ASC | DESC ]]
    [LIMIT row_count];

    ---> Example
    UPDATE employees SET isConfirmed=1 ORDER BY joiningDate LIMIT 10

In the above example, 10 rows will be updated according to the order of employees `joiningDate`.

## Multiple Table UPDATE
In multiple table `UPDATE`, it updates rows in each specified tables that satisfy the conditions. Each matching row is updated once, even if it matches the conditions multiple times.

In multiple table `UPDATE`, `ORDER BY` and `LIMIT` cannot be used.

Syntax for multi table `UPDATE` is,

    UPDATE [LOW_PRIORITY] [IGNORE] 
    table1, table2, ...
        SET column1 = expression1,
            column2 = expression2,
            ...
        [WHERE conditions]

For example consider two tables, `products` and `salesOrders`. In case, we decrease the quantity of a particular product from the sales order which is placed already. Then we also need to increase that quantity in our stock column of `products` table. This can be done in single SQL update statement like below.

    UPDATE products, salesOrders
      SET salesOrders.Quantity = salesOrders.Quantity - 5, 
          products.availableStock = products.availableStock + 5
    WHERE products.productId = salesOrders.productId
      AND salesOrders.orderId = 100 AND salesOrders.productId = 20;

In the above example, quantity '5' will be reduced from the `salesOrders` table and the same will be increased in `products` table according to the `WHERE` conditions.


## Bulk UPDATE
When updating multiple rows with different values it is much quicker to use a bulk update.

    UPDATE people 
    SET name = 
      (CASE id WHEN 1 THEN 'Karl'
               WHEN 2 THEN 'Tom'
               WHEN 3 THEN 'Mary'
       END)
    WHERE id IN (1,2,3);

By bulk updating only one query can be sent to the server instead of one query for each row to update. The cases should contain all possible parameters looked up in the `WHERE` clause.


