---
title: "ResultSet"
slug: "resultset"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

A ResultSet object maintains a cursor pointing to its current row of data. Initially the cursor is positioned before the first row. The next method moves the cursor to the next row, and because it returns false when there are no more rows in the ResultSet object, it can be used in a while loop to iterate through the result se

## ResultSet
To create a `ResultSet` you should to create a `Statement` or `PrepapredStatement` :

# Create ResultSet with Statement

    try {
        Class.forName(driver);
        Connection connection = DriverManager.getConnection(
                "jdbc:somedb://localhost/databasename", "username", "password");
        Statement statement = connection.createStatement();
        ResultSet result = statement.executeQuery("SELECT * FROM my_table");

    } catch (ClassNotFoundException | SQLException e) {
    }

# Create ResultSet with PrepapredStatement

    try {
        Class.forName(driver);
        Connection connection = DriverManager.getConnection(
                "jdbc:somedb://localhost/databasename", "username", "password");
        PreparedStatement preparedStatement = connection.prepareStatement("SELECT * FROM my_table");
        ResultSet result = preparedStatement.executeQuery();

    } catch (ClassNotFoundException | SQLException e) {
    }

# Check if your ResultSet have information or not

    if (result.next()) {
       //yes result not empty                
    }

# Get information from ResultSet

There are several type of information you can get from your `ResultSet` like `String, int, boolean, float, Blob`, ... to get information you had to use a loop or a simple if :

    if (result.next()) {
        //get int from your result set
        result.getInt("id");
        //get string from your result set
        result.getString("username");
        //get boolean from your result set
        result.getBoolean("validation");
        //get double from your result set
        result.getDouble("price");
    }

