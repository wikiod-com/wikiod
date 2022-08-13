---
title: "SELECT"
slug: "select"
draft: false
images: []
weight: 9580
type: docs
toc: true
---

`SELECT` is used to retrieve rows selected from one or more tables.

## Syntax
 - SELECT DISTINCT [expressions] FROM TableName [WHERE conditions]; ///Simple Select
 - SELECT DISTINCT(a), b ...  is the same as SELECT DISTINCT a, b ...

- SELECT [ ALL | DISTINCT | DISTINCTROW ]
       [ HIGH_PRIORITY ]
       [ STRAIGHT_JOIN ]
       [ SQL_SMALL_RESULT | SQL_BIG_RESULT ] [ SQL_BUFFER_RESULT ]
       [ SQL_CACHE | SQL_NO_CACHE ]
       [ SQL_CALC_FOUND_ROWS ]
expressions
FROM tables
[WHERE conditions]
[GROUP BY expressions]
[HAVING condition]
[ORDER BY expression [ ASC | DESC ]]
[LIMIT [offset_value] number_rows | LIMIT number_rows OFFSET offset_value]
[PROCEDURE procedure_name]
[INTO [ OUTFILE 'file_name' options 
       | DUMPFILE 'file_name'
       | @variable1, @variable2, ... @variable_n]
[FOR UPDATE | LOCK IN SHARE MODE]; ///Full Select Syntax

For more information on MySQL's `SELECT` statement, refer [MySQL Docs][1].

  [1]: https://dev.mysql.com/doc/refman/5.7/en/select.html

## SELECT with DISTINCT
The `DISTINCT` clause after `SELECT` eliminates duplicate rows from the result set. 

    CREATE TABLE `car`
    (   `car_id` INT UNSIGNED NOT NULL PRIMARY KEY, 
        `name` VARCHAR(20), 
        `price` DECIMAL(8,2)
    );
    
    INSERT INTO CAR (`car_id`, `name`, `price`) VALUES (1, 'Audi A1', '20000');
    INSERT INTO CAR (`car_id`, `name`, `price`) VALUES (2, 'Audi A1', '15000');
    INSERT INTO CAR (`car_id`, `name`, `price`) VALUES (3, 'Audi A2', '40000');
    INSERT INTO CAR (`car_id`, `name`, `price`) VALUES (4, 'Audi A2', '40000');
    
    SELECT DISTINCT `name`, `price` FROM CAR;
    +---------+----------+
    | name    | price    |
    +---------+----------+
    | Audi A1 | 20000.00 |
    | Audi A1 | 15000.00 |
    | Audi A2 | 40000.00 |
    +---------+----------+

`DISTINCT` works across all columns to deliver the results, not individual columns. The latter is often a misconception of new SQL developers. In short, it is the distinctness at the row-level of the result set that matters, not distinctness at the column-level. To visualize this, look at "Audi A1" in the above result set.


For later versions of MySQL, `DISTINCT` has implications with its use alongside `ORDER BY`. The setting for `ONLY_FULL_GROUP_BY` comes into play as seen in the following MySQL Manual Page entitled [MySQL Handling of GROUP BY][1].

[1]: http://dev.mysql.com/doc/refman/5.7/en/group-by-handling.html

## SELECT all columns (*)
**Query**    

    SELECT * FROM stack;

**Result**

    +------+----------+----------+
    | id   | username | password |
    +------+----------+----------+
    |    1 | admin    | admin    |
    |    2 | stack    | stack    |
    +------+----------+----------+
    2 rows in set (0.00 sec)

You can select all columns from one table in a join by doing:
  

    SELECT stack.* FROM stack JOIN Overflow ON stack.id = Overflow.id;


**Best Practice** Do not use `*` unless you are debugging or fetching the row(s) into associative arrays, otherwise schema changes (ADD/DROP/rearrange columns) can lead to nasty application errors. Also, if you give the list of columns you need in your result set, MySQL's query planner often can optimize the query.


**Pros:**

1. When you add/remove columns, you don't have to make changes where you did use `SELECT *`
2. It's shorter to write
3. You also see the answers, so can `SELECT *`-usage ever be justified?

**Cons:**

1. You are returning more data than you need. Say you add a VARBINARY column that contains 200k per row. You only need this data in one place for a single record - using `SELECT *` you can end up returning 2MB per 10 rows that you don't need
2. Explicit about what data is used
3. Specifying columns means you get an error when a column is removed
4. The query processor has to do some more work - figuring out what columns exist on the table (thanks @vinodadhikary)
5. You can find where a column is used more easily
6. You get all columns in joins if you use SELECT *
7. You can't safely use ordinal referencing (though using ordinal references for columns is bad practice in itself)
8. In complex queries with `TEXT` fields, the query may be slowed down by less-optimal temp table processing


## SELECT by column name
    CREATE TABLE stack(
        id INT,
        username VARCHAR(30) NOT NULL,
        password VARCHAR(30) NOT NULL
    );

    INSERT INTO stack (`id`, `username`, `password`) VALUES (1, 'Foo', 'hiddenGem');
    INSERT INTO stack (`id`, `username`, `password`) VALUES (2, 'Baa', 'verySecret');


**Query**

    SELECT id FROM stack;

**Result**

    +------+
    | id   |
    +------+
    |    1 |
    |    2 |
    +------+


## SELECT with LIKE (%)
    CREATE TABLE stack
    (  id int AUTO_INCREMENT PRIMARY KEY,
       username VARCHAR(100) NOT NULL
    );
    
    INSERT stack(username) VALUES 
    ('admin'),('k admin'),('adm'),('a adm b'),('b XadmY c'), ('adm now'), ('not here'); 

"adm" anywhere:

    SELECT * FROM stack WHERE username LIKE "%adm%";  
    +----+-----------+
    | id | username  |
    +----+-----------+
    |  1 | admin     |
    |  2 | k admin   |
    |  3 | adm       |
    |  4 | a adm b   |
    |  5 | b XadmY c |
    |  6 | adm now   |
    +----+-----------+

Begins with "adm":

    SELECT * FROM stack WHERE username LIKE "adm%";
    +----+----------+
    | id | username |
    +----+----------+
    |  1 | admin    |
    |  3 | adm      |
    |  6 | adm now  |
    +----+----------+

Ends with "adm":

    SELECT * FROM stack WHERE username LIKE "%adm"; 
    +----+----------+
    | id | username |
    +----+----------+
    |  3 | adm      |
    +----+----------+

Just as the `%` character in a `LIKE` clause matches any number of characters, the `_` character matches just one character. For example, 

    SELECT * FROM stack WHERE username LIKE "adm_n"; 
    +----+----------+
    | id | username |
    +----+----------+
    |  1 | admin    |
    +----+----------+


**Performance Notes** If there is an index on `username`, then

 - `LIKE 'adm'` performs the same as `= 'adm'
 - `LIKE 'adm%` is a "range", similar to `BETWEEN..AND..` It can make good use of an index on the column. 
 - `LIKE '%adm'` (or any variant with a _leading_ wildcard) cannot use any index. Therefore it will be slow. On tables with many rows, it is likely to be so slow it is useless.
 - `RLIKE` (`REGEXP`) tends to be slower than `LIKE`, but has more capabilities.
 - While MySQL offers `FULLTEXT` indexing on many types of table and column, those `FULLTEXT` indexes are *not* used to fulfill queries using `LIKE`.





## SELECT with CASE or IF
**Query**

    SELECT st.name,
           st.percentage, 
           CASE WHEN st.percentage >= 35 THEN 'Pass' ELSE 'Fail' END AS `Remark` 
    FROM student AS st ;

**Result**

    +--------------------------------+
    |   name   | percentage | Remark |
    +--------------------------------+
    |   Isha   |     67     |  Pass  |
    |   Rucha  |     28     |  Fail  |
    |   Het    |     35     |  Pass  |
    |   Ansh   |     92     |  Pass  |
    +--------------------------------+

**Or with IF**

    SELECT st.name,
           st.percentage, 
           IF(st.percentage >= 35, 'Pass', 'Fail') AS `Remark` 
    FROM student AS st ;

**N.B**

    IF(st.percentage >= 35, 'Pass', 'Fail')

> This means : IF st.percentage >= 35 is **TRUE** then return `'Pass'` ELSE
> return `'Fail'`

## SELECT with Alias (AS)
SQL aliases are used to temporarily rename a table or a column. They are generally used to improve readability.


**Query**

    SELECT username AS val FROM stack; 
    SELECT username val FROM stack;
(Note:  `AS` is syntactically optional.)

**Result**

    +-------+
    | val   |
    +-------+
    | admin |
    | stack |
    +-------+
    2 rows in set (0.00 sec)

## SELECT with a LIMIT clause
**Query:**

     SELECT *
       FROM Customers
      ORDER BY CustomerID 
      LIMIT 3;

**Result:**

<table>
  <tbody><tr>
    <th>CustomerID</th>
    <th>CustomerName</th>
    <th>ContactName</th>
    <th>Address</th>
    <th>City</th>
    <th>PostalCode</th>
    <th>Country</th>
  </tr>
  <tr>
    <td>1<br><br></td>
    <td>Alfreds Futterkiste</td>
    <td>Maria Anders</td>
    <td>Obere Str. 57</td>
    <td>Berlin</td>
    <td>12209</td>
    <td>Germany</td>
  </tr>
  <tr>
    <td>2</td>
    <td>Ana Trujillo Emparedados y helados</td>
    <td>Ana Trujillo</td>
    <td>Avda. de la Constitución 2222</td>
    <td>México D.F.</td>
    <td>05021</td>
    <td>Mexico</td>
  </tr>
  <tr>
    <td>3</td>
    <td>Antonio Moreno Taquería</td>
    <td>Antonio Moreno</td>
    <td>Mataderos 2312</td>
    <td>México D.F.</td>
    <td>05023</td>
    <td>Mexico</td>
  </tr>
</tbody>
</table>

**Best Practice** Always use `ORDER BY` when using `LIMIT`; otherwise the rows you will get will be unpredictable.

**Query:**

     SELECT *
       FROM Customers
      ORDER BY CustomerID 
      LIMIT 2,1;


**Explanation:**

When a `LIMIT` clause contains two numbers, it is interpreted as `LIMIT offset,count`. So, in this example the query skips two records and returns one. 

**Result:**


<table>
  <tbody><tr>
    <th>CustomerID</th>
    <th>CustomerName</th>
    <th>ContactName</th>
    <th>Address</th>
    <th>City</th>
    <th>PostalCode</th>
    <th>Country</th>
  </tr>
  <tr>
    <td>3</td>
    <td>Antonio Moreno Taquería</td>
    <td>Antonio Moreno</td>
    <td>Mataderos 2312</td>
    <td>México D.F.</td>
    <td>05023</td>
    <td>Mexico</td>
  </tr>
</tbody>
</table>


**Note:**

The values in `LIMIT` clauses must be constants; they may not be column values.


## SELECT with WHERE
**Query**

    SELECT * FROM stack WHERE username = "admin" AND password = "admin";

**Result**

    +------+----------+----------+
    | id   | username | password |
    +------+----------+----------+
    |    1 | admin    | admin    |
    +------+----------+----------+
    1 row in set (0.00 sec) 

# Query with a nested SELECT in the WHERE clause
The `WHERE` clause can contain any valid `SELECT` statement to write more complex queries. This is a 'nested' query

**Query**

Nested queries are usually used to return single atomic values from queries for comparisons.

    SELECT title FROM books WHERE author_id = (SELECT id FROM authors WHERE last_name = 'Bar' AND first_name = 'Foo');

Selects all usernames with no email address

    SELECT * FROM stack WHERE username IN (SELECT username FROM signups WHERE email IS NULL);

Disclaimer: Consider using [joins][1] for performance improvements when comparing a whole result set. 


  [1]: http://stackoverflow.com/questions/17946221/sql-join-and-different-types-of-joins

## SELECT with BETWEEN
You can use BETWEEN clause to replace a combination of "greater than equal AND less than equal" conditions.

**Data**

    +----+-----------+
    | id | username  |
    +----+-----------+
    |  1 | admin     |
    |  2 | root      |
    |  3 | toor      |
    |  4 | mysql     |
    |  5 | thanks    |
    |  6 | java      |
    +----+-----------+

**Query with operators**

    SELECT * FROM stack WHERE id >= 2 and id <= 5; 

**Similar query with BETWEEN**

    SELECT * FROM stack WHERE id BETWEEN 2 and 5; 

**Result**

    +----+-----------+
    | id | username  |
    +----+-----------+
    |  2 | root      |
    |  3 | toor      |
    |  4 | mysql     |
    |  5 | thanks    |
    +----+-----------+
    4 rows in set (0.00 sec)

**Note**

> **BETWEEN uses `>=` and `<=`, not `>` and `<`.** 

**Using NOT BETWEEN**

If you want to use the negative you can use `NOT`. For example :

    SELECT * FROM stack WHERE id NOT BETWEEN 2 and 5; 

**Result**

    +----+-----------+
    | id | username  |
    +----+-----------+
    |  1 | admin     |
    |  6 | java      |
    +----+-----------+
    2 rows in set (0.00 sec)

**Note**

> **NOT BETWEEN uses `>` and `<` and not `>=` and `<=`** That is, `WHERE id NOT BETWEEN 2 and 5` is the same as `WHERE (id < 2 OR id > 5)`.

If you have an index on a column you use in a `BETWEEN` search, MySQL can use that index for a range scan. 

## SELECT with LIKE(_)
A `_` character in a `LIKE` clause pattern matches a single character. 

**Query** 

    SELECT username FROM users WHERE users LIKE 'admin_';

**Result**

    +----------+
    | username |  
    +----------+
    | admin1   |
    | admin2   |
    | admin-   |
    | adminA   |
    +----------+

## SELECT with date range
    SELECT ... WHERE dt >= '2017-02-01'
                 AND dt  < '2017-02-01' + INTERVAL 1 MONTH

Sure, this could be done with `BETWEEN` and inclusion of `23:59:59`.  But, the pattern has this benefits:

* You don't have pre-calculate the end date (which is often an exact length from the start)
* You don't include both endpoints (as `BETWEEN` does), nor type '23:59:59' to avoid it.
* It works for `DATE`, `TIMESTAMP`, `DATETIME`, and even the microsecond-included `DATETIME(6)`.
* It takes care of leap days, end of year, etc.
* It is index-friendly (so is `BETWEEN`).

