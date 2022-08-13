---
title: "PHP MySQLi"
slug: "php-mysqli"
draft: false
images: []
weight: 9487
type: docs
toc: true
---

The [`mysqli` interface](http://php.net/manual/en/book.mysqli.php) is an improvement (it means "MySQL Improvement extension") of the `mysql` interface, which was deprecated in version 5.5 and is removed in version 7.0.
The mysqli extension, or as it is sometimes known, the MySQL improved extension, was developed to take advantage of new features found in MySQL systems versions 4.1.3 and newer. The mysqli extension is included with PHP versions 5 and later.

# Features

The mysqli interface has a number of benefits, the key enhancements over the mysql extension being:

- Object-oriented interface
- Support for Prepared Statements
- Support for Multiple Statements
- Support for Transactions
- Enhanced debugging capabilities
- Embedded server support
 
It features a [dual interface](http://php.net/manual/en/mysqli.quickstart.dual-interface.php): the older, procedural style and a new, [object-oriented programming (OOP)](https://en.wikipedia.org/wiki/Object-oriented_programming) style. The deprecated `mysql` had only a procedural interface, so the object-oriented style is often preferred. However, the new style is also favorable because of the power of OOP.

# Alternatives 

An alternative to the `mysqli` interface to access databases is the newer [PHP Data Objects (PDO)][1] interface. This features only OOP-style programming and can access more than only MySQL-type databases.


  [1]: https://www.wikiod.com/php

## Close connection
When we are finished querying the database, it is recommended to close the connection to free up resources. 

**Object oriented style**

    $conn->close();

**Procedural style**

    mysqli_close($conn);

**Note**: The connection to the server will be closed as soon as the execution of the script ends, unless it's closed earlier by explicitly calling the close connection function. 

**Use Case:** If our script has a fair amount of processing to perform after fetching the result and has retrieved the full result set, we definitely should close the connection. If we were not to, there's a chance the MySQL server will reach its connection limit when the web server is under heavy use.

## MySQLi connect
**Object oriented style**

Connect to Server

    $conn = new mysqli("localhost","my_user","my_password");

*Set the default database:* `$conn->select_db("my_db");`

Connect to Database

    $conn = new mysqli("localhost","my_user","my_password","my_db");

**Procedural style**

Connect to Server

    $conn = mysqli_connect("localhost","my_user","my_password");

*Set the default database:* `mysqli_select_db($conn, "my_db");`

Connect to Database

    $conn = mysqli_connect("localhost","my_user","my_password","my_db");

**Verify Database Connection**

Object oriented style

    if ($conn->connect_errno > 0) {
        trigger_error($db->connect_error);
    } // else: successfully connected

Procedural style

    if (!$conn) {
       trigger_error(mysqli_connect_error());
    } // else: successfully connected



## Loop through MySQLi results
PHP makes it easy to get data from your results and loop over it using a `while` statement. When it fails to get the next row, it returns `false`, and your loop ends. These examples work with

- [mysqli_fetch_assoc](http://php.net/manual/en/mysqli-result.fetch-assoc.php) - Associative array with column names as keys
- [mysqli_fetch_object](http://php.net/manual/en/mysqli-result.fetch-object.php) - `stdClass` object with column names as variables
- [mysqli_fetch_array](http://php.net/manual/en/mysqli-result.fetch-array.php) - Associative AND Numeric array (can use arguments to get one or the other)
- [mysqli_fetch_row](http://php.net/manual/en/mysqli-result.fetch-row.php) - Numeric  array

**Object oriented style**

    while($row = $result->fetch_assoc()) {
        var_dump($row);
    }

**Procedural style**

    while($row = mysqli_fetch_assoc($result)) {
        var_dump($row);
    }

To get exact information from results, we can use:

    while ($row = $result->fetch_assoc()) {
        echo 'Name and surname: '.$row['name'].' '.$row['surname'].'<br>';
        echo 'Age: '.$row['age'].'<br>'; // Prints info from 'age' column
    }

## Prepared statements in MySQLi
Please read https://www.wikiod.com/php/pdo#Preventing SQL injection with Parameterized Queries for a complete discussion of why prepared statements help you secure your SQL statements from SQL Injection attacks

The `$conn` variable here is a MySQLi object. See [MySQLi connect example][1] for more details.

For both examples, we assume that `$sql` is

    $sql = "SELECT column_1 
        FROM table 
        WHERE column_2 = ? 
            AND column_3 > ?";

The `?` represents the values we will provide later. Please note that we do not need quotes for the placeholders, regardless of the type. We can also only provide placeholders in the data portions of the query, meaning `SET`, `VALUES` and `WHERE`. You cannot use placeholders in the `SELECT` or `FROM` portions.

**Object oriented style**

    if ($stmt = $conn->prepare($sql)) {
      $stmt->bind_param("si", $column_2_value, $column_3_value);
      $stmt->execute();

      $stmt->bind_result($column_1);
      $stmt->fetch();
      //Now use variable $column_1 one as if it were any other PHP variable
      $stmt->close();
    }

**Procedural style**

    if ($stmt = mysqli_prepare($conn, $sql)) {
      mysqli_stmt_bind_param($stmt, "si", $column_2_value, $column_3_value);
      mysqli_stmt_execute($stmt);
      // Fetch data here
      mysqli_stmt_close($stmt);
    }

The first parameter of `$stmt->bind_param` or the second parameter of `mysqli_stmt_bind_param` is determined by the data type of the corresponding parameter in the SQL query:

| Parameter | Data type of the bound parameter |
| --------- | -------------------------------- |
| `i`       | integer                          |
| `d`       | double                           |
| `s`       | string                           |
| `b`       | blob                             |

Your list of parameters needs to be in the order provided in your query.
In this example `si` means the first parameter *(`column_2 = ?`)* is string and the second parameter *(`column_3 > ?`)* is integer.

For retrieving data, see https://www.wikiod.com/php/php-mysqli#How to get data from a prepared statement

  [1]: https://www.wikiod.com/php/php-mysqli#MySQLi connect

## Escaping Strings
Escaping strings is an older (**and less secure**) method of securing data for insertion into a query. It works by using [MySQL's function mysql_real_escape_string()](http://dev.mysql.com/doc/refman/5.7/en/mysql-real-escape-string.html) to process and sanitize the data (in other words, PHP is not doing the escaping). The MySQLi API provides direct access to this function

    $escaped = $conn->real_escape_string($_GET['var']);
    // OR
    $escaped = mysqli_real_escape_string($conn, $_GET['var']);

At this point, you have a string that MySQL considers to be safe for use in a direct query

    $sql = 'SELECT * FROM users WHERE username = "' . $escaped . '"';
    $result = $conn->query($sql);

So why is this not as secure as [prepared statements](https://www.wikiod.com/php/php-mysqli#Prepared statements in MySQLi)? There are ways to trick MySQL to produce a string it considers safe. Consider the following example

    $id = mysqli_real_escape_string("1 OR 1=1");    
    $sql = 'SELECT * FROM table WHERE id = ' . $id;

`1 OR 1=1` does not represent data that MySQL will escape, yet this still represents SQL injection. There are [other examples](http://stackoverflow.com/questions/5741187/sql-injection-that-gets-around-mysql-real-escape-string) as well that represent places where it returns unsafe data. The problem is that MySQL's escaping function is designed to **make data comply with SQL syntax**. It's NOT designed to make sure that **MySQL can't confuse user data for SQL instructions**.

## MySQLi query
The `query` function takes a valid SQL string and executes it directly against the database connection `$conn`

**Object oriented style**

    $result = $conn->query("SELECT * FROM `people`");

**Procedural style**

    $result = mysqli_query($conn, "SELECT * FROM `people`");

>**CAUTION**

A common problem here is that people will simply execute the query and expect it to work (i.e. return a [mysqli_stmt object](https://secure.php.net/manual/en/class.mysqli-stmt.php)). Since this function takes only a string, you're building the query first yourself. If there are any mistakes in the SQL at all, the MySQL compiler will fail, **at which point this function will return `false`**.

    $result = $conn->query('SELECT * FROM non_existent_table'); // This query will fail
    $row = $result->fetch_assoc();

The above code will generate a `E_FATAL` error because `$result` is `false`, and not an object. 

> PHP Fatal error: Call to a member function fetch_assoc() on a non-object

The procedural error is similar, but not fatal, because we're just violating the expectations of the function. 

    $row = mysqli_fetch_assoc($result); // same query as previous

You will get the following message from PHP

> mysqli_fetch_array() expects parameter 1 to be mysqli_result, boolean given

You can avoid this by doing a test first

    if($result) $row = mysqli_fetch_assoc($result);

## Debugging SQL in MySQLi
So your query has failed (see https://www.wikiod.com/php/php-mysqli#MySQLi connect for how we made `$conn`)

    $result = $conn->query('SELECT * FROM non_existent_table'); // This query will fail

How do we find out what happened? `$result` is `false` so that's no help. Thankfully the connect `$conn` can tell us what MySQL told us about the failure

    trigger_error($conn->error);

or procedural

    trigger_error(mysqli_error($conn));

You should get an error similar to

> Table 'my_db.non_existent_table' doesn't exist



## How to get data from a prepared statement
# Prepared statements

See [Prepared statements in MySQLi](https://www.wikiod.com/php/php-mysqli#Prepared statements in MySQLi) for how to prepare and execute a query.

# Binding of results

**Object-oriented style**

    $stmt->bind_result($forename);

**Procedural style**

    mysqli_stmt_bind_result($stmt, $forename);

The problem with using `bind_result` is that it requires the statement to specify the columns that will be used. This means that for the above to work the query must have looked like this `SELECT forename FROM users`. To include more columns simply add them as parameters to the `bind_result` function (and ensure that you add them to the SQL query).

In both cases, we're assigning the `forename` column to the `$forename` variable. These functions take as many arguments as columns you want to assign. The assignment is only done once, since the function binds by reference. 

We can then loop as follows:

**Object-oriented style**

    while ($stmt->fetch())
        echo "$forename<br />";

**Procedural style**

    while (mysqli_stmt_fetch($stmt))
        echo "$forename<br />";

The drawback to this is that you have to assign a lot of variables at once. This makes keeping track of large queries difficult. If you have [MySQL Native Driver (`mysqlnd`)](http://php.net/manual/en/book.mysqlnd.php) installed, all you need to do is use [get_result](http://php.net/manual/en/mysqli-stmt.get-result.php).

**Object-oriented style**

    $result = $stmt->get_result();

**Procedural style**

    $result = mysqli_stmt_get_result($stmt);

This is **much** easier to work with because now we're getting a [mysqli_result](http://php.net/manual/en/class.mysqli-result.php) object. This is the same object that [mysqli_query returns](https://www.wikiod.com/php/php-mysqli#MySQLi query). This means you can use a [regular result loop](https://www.wikiod.com/php/php-mysqli#Loop through MySQLi results) to get your data.

---

# What if I cannot install `mysqlnd`?

If that is the case then @Sophivorus has you covered with [this amazing answer](http://stackoverflow.com/a/30551477/3578036).

This function can perform the task of `get_result` without it being installed on the server. It simply loops through the results and builds an associative array

    function get_result(\mysqli_stmt $statement)
    {
        $result = array();
        $statement->store_result();
        for ($i = 0; $i < $statement->num_rows; $i++)
        {
            $metadata = $statement->result_metadata();
            $params = array();
            while ($field = $metadata->fetch_field())
            {
                $params[] = &$result[$i][$field->name];
            }
            call_user_func_array(array($statement, 'bind_result'), $params);
            $statement->fetch();
        }
        return $result;
    }

We can then use the function to get results like this, just as if we were using `mysqli_fetch_assoc()`

    <?php
    $query = $mysqli->prepare("SELECT * FROM users WHERE forename LIKE ?");
    $condition = "J%";
    $query->bind_param("s", $condition);
    $query->execute();
    $result = get_result($query);

    while ($row = array_shift($result)) {
        echo $row["id"] . ' - ' . $row["forename"] . ' ' . $row["surname"] . '<br>';
    }

It will have the same output as if you were using the `mysqlnd` driver, except it does not have to be installed. This is very useful if you are unable to install said driver on your system. Just implement this solution.

## MySQLi Insert ID
Retrieve the last ID generated by an [`INSERT`][1] query on a table with an [AUTO_INCREMENT][3] column.

**Object-oriented Style**

    $id = $conn->insert_id;

**Procedural Style**

    $id = mysqli_insert_id($conn);

> Returns zero if there was no previous query on the connection or if the query did not update an AUTO_INCREMENT value.

**Insert id when updating rows**

Normally an `UPDATE` statement does not return an insert id, since an `AUTO_INCREMENT` id is only returned when a new row has been saved (or inserted). One way of making updates to the new id is to use `INSERT ... ON DUPLICATE KEY UPDATE` syntax for updating.

Setup for examples to follow:

    CREATE TABLE iodku (
        id INT AUTO_INCREMENT NOT NULL,
        name VARCHAR(99) NOT NULL,
        misc INT NOT NULL,
        PRIMARY KEY(id),
        UNIQUE(name)
    ) ENGINE=InnoDB;
    
    INSERT INTO iodku (name, misc)
        VALUES
        ('Leslie', 123),
        ('Sally', 456);
    Query OK, 2 rows affected (0.00 sec)
    Records: 2  Duplicates: 0  Warnings: 0
    +----+--------+------+
    | id | name   | misc |
    +----+--------+------+
    |  1 | Leslie |  123 |
    |  2 | Sally  |  456 |
    +----+--------+------+

The case of IODKU performing an "update" and `LAST_INSERT_ID()` retrieving the relevant `id`:

    $sql = "INSERT INTO iodku (name, misc)
        VALUES
        ('Sally', 3333)            -- should update
        ON DUPLICATE KEY UPDATE    -- `name` will trigger "duplicate key"
        id = LAST_INSERT_ID(id),
        misc = VALUES(misc)";
    $conn->query($sql);
    $id = $conn->insert_id;        -- picking up existing value (2)

The case where IODKU performs an "insert" and `LAST_INSERT_ID()` retrieves the new `id`:

    $sql = "INSERT INTO iodku (name, misc)
        VALUES
        ('Dana', 789)            -- Should insert
        ON DUPLICATE KEY UPDATE
        id = LAST_INSERT_ID(id),
        misc = VALUES(misc);
    $conn->query($sql);
    $id = $conn->insert_id;      -- picking up new value (3)

Resulting table contents:

    SELECT * FROM iodku;
    +----+--------+------+
    | id | name   | misc |
    +----+--------+------+
    |  1 | Leslie |  123 |
    |  2 | Sally  | 3333 |  -- IODKU changed this
    |  3 | Dana   |  789 |  -- IODKU added this
    +----+--------+------+

  [1]: https://www.wikiod.com/sql/insert
  [2]: https://www.wikiod.com/sql/update
  [3]: https://www.wikiod.com/sql/primary-keys#Using Auto Increment





