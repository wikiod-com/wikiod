---
title: "Prepare and Execute your SQL statements"
slug: "prepare-and-execute-your-sql-statements"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

**Warning**

Prepared statement cannot care a wild parameter for the table names. For exemple this following statement is not correct :

    $query = "SELECT name, city FROM ? WHERE id = ? AND country = ?";

The correct prepared query would be :

    $query = "SELECT name, city FROM users WHERE id = ? AND country = ?";


## Usage
    // 1. Connect to the database (this example with MySQL)
    $host = 'localhost';
    $database = 'users';
    $user = 'root';
    $password = '';
    $dsn = "mysql:host=$host;dbname=$database";
    $pdo = new PDO($dsn, $user, $password);
    
    // 2. Prepare your query
    
    // 2.1 First way
    $query_1 = "SELECT name, city FROM users WHERE id = ? AND country = ?";
    $statement_1 = $pdo->prepare($query_1);
    
    // 2.2 Second way
    $query_2 = "SELECT name, city FROM users WHERE id = :id AND country = :country";
    $statement_2 = $pdo->prepare($query_2);
    
    // 3. Execute your query
    
    // 3.1 With the first way
    $statement_1->execute([1, 'US']);
    
    // 3.2 With the second way
    $statement_2->execute([
        ':id' => 1,
        ':country' => 'US'
    ]);
    
    // 4. Fetch your data
    $data_1 = $statement_1->fetchAll();
    $data_2 = $statement_2->fetchAll();

