---
title: "PreparedStatement"
slug: "preparedstatement"
draft: false
images: []
weight: 9905
type: docs
toc: true
---

A `PreparedStatement` declares the statement before it is executed, and allows for placeholders for parameters. This allows the statement to be prepared (and optimized) once on the server, and then reused with different sets of parameters.

The added benefit of the parameter placeholders, is that it provides protection against SQL injection. This is achieved either by sending the parameter values separately, or because the driver escapes values correctly as needed.

## Basic usage of a prepared statement
<!-- language-all: java -->
This example shows how to create a prepared statement with an insert statement with parameters, set values to those parameters and then executing the statement.

    Connection connection = ... // connection created earlier
    try (PreparedStatement insert = connection.prepareStatement(
            "insert into orders(id, customerid, totalvalue, comment) values (?, ?, ?, ?)")) {
        //NOTE: Position indexes start at 1, not 0
        insert.setInt(1, 1);
        insert.setInt(2, 7934747);
        insert.setBigDecimal(3, new BigDecimal("100.95"));
        insert.setString(4, "quick delivery requested");
    
        insert.executeUpdate();
    }

The question marks (`?`) in the insert statement are the parameter placeholders. They are positional parameters that are later referenced (using a 1-based index) using the `setXXX` methods to set values to those parameters.

The use of try-with-resources ensures that the statement is closed and any resources in use for that statement are released.

## Setting parameters for PreparedStatement


