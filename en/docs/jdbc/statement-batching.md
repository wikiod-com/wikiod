---
title: "Statement batching"
slug: "statement-batching"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Statement batching is either executing multiple statements as one unit (with a normal `java.sql.Statement`), or a single statement with multiple sets of parameter values (with a `java.sql.PreparedStatement`).



Statement batching allows a program to collect related statement, or in the case of prepared statements related parameter value sets, and send them to the database server as a single execute.

The benefits of statement batching can include improved performance. If and how these performance benefits are achieved depends on the driver and database support, but they include:

* Sending all statements (or all values sets) in one command
* Rewriting the statement(s) so they can be executed like one big statement

## Batch insertion using PreparedStatement
<!-- language-all: java -->
Batch execution using [`java.sql.PreparedStatement`][1] allows you to execute a single DML statement with multiple sets of values for its parameters.

This example demonstrates how to prepare an insert statement and use it to insert multiple rows in a batch.

    Connection connection = ...; // obtained earlier
    connection.setAutoCommit(false); // disabling autoCommit is recommend for batching
    int orderId = ...; // The primary key of inserting and order
    List<OrderItem> orderItems = ...; // Order item data
    
    try (PreparedStatement insert = connection.prepareStatement(
            "INSERT INTO orderlines(orderid, itemid, quantity) VALUES (?, ?, ?)")) {
        // Add the order item data to the batch
        for (OrderItem orderItem : orderItems) {
            insert.setInt(1, orderId);
            insert.setInt(2, orderItem.getItemId());
            insert.setInt(3, orderItem.getQuantity());
            insert.addBatch();
        }
    
        insert.executeBatch();//executing the batch 
    }
    
    connection.commit();//commit statements to apply changes 


  [1]: https://docs.oracle.com/javase/8/docs/api/java/sql/PreparedStatement.html

## Batch execution using Statement
<!-- language-all: java -->
Batch execution using [`java.sql.Statement`][1] allows you to execute multiple DML statements (`update`, `insert`, `delete`) at once. This is achieved by creating a single statement object, adding the statements to execute, and then execute the batch as one.

    Connection connection = ...; // obtained earlier
    connection.setAutoCommit(false); // disabling autocommit is recommended for batch execution
    
    try (Statement statement = connection.createStatement()) {
        statement.addBatch("INSERT INTO users (id, username) VALUES (2, 'anna')");
        statement.addBatch("INSERT INTO userrole(userid, rolename) VALUES (2, 'admin')");
        
        statement.executeBatch();//executing the batch 
    }
    
    connection.commit();//commit statements to apply changes 

**Note:**

`statement.executeBatch();` will return `int[]` to hold returned values, you can execute your batch like this :

    int[] stmExc = statement.executeBatch();//executing the batch 

  [1]: https://docs.oracle.com/javase/8/docs/api/java/sql/Statement.html

