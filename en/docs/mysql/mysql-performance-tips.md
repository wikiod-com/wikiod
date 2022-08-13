---
title: "Mysql Performance Tips"
slug: "mysql-performance-tips"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Optimizing Storage Layout for InnoDB Tables
 1. In InnoDB, having a long PRIMARY KEY (either a single column with a 
    lengthy value, or several columns that form a long composite value) 
    wastes a lot of disk space. The primary key value for a row is duplicated in all the secondary index records that point to the same
    row. Create an AUTO_INCREMENT column as the primary key if your
    primary key is long.
 2. Use the VARCHAR data type instead of CHAR to store variable-length
    strings or for columns with many NULL values. A CHAR(N) column
    always takes N characters to store data, even if the string is
    shorter or its value is NULL. Smaller tables fit better in the
    buffer pool and reduce disk I/O.

> When using COMPACT row format (the default InnoDB format) and
> variable-length character sets, such as utf8 or sjis, CHAR(N) columns
> occupy a variable amount of space, but still at least N bytes.

 3. For tables that are big, or contain lots of repetitive text or
    numeric data, consider using COMPRESSED row format. Less disk I/O is
    required to bring data into the buffer pool, or to perform full
    table scans. Before making a permanent decision, measure the amount
    of compression you can achieve by using COMPRESSED versus COMPACT
    row format.  _Caveat:_  Benchmarks rarely show better than 2:1 compression
    and there is a lot of overhead in the buffer_pool for COMPRESSED.
 4. Once your data reaches a stable size, or a growing table has
    increased by tens or some hundreds of megabytes, consider using the
    OPTIMIZE TABLE statement to reorganize the table and compact any
    wasted space. The reorganized tables require less disk I/O to
    perform full table scans. This is a straightforward technique that
    can improve performance when other techniques such as improving
    index usage or tuning application code are not practical.
    _Caveat_:  Regardless of table size, OPTIMIZE TABLE should only rarely be performed.
    This is because it is costly, and rarely improves the table enough to be worth it.
    InnoDB is reasonably good at keeping its B+Trees free of a lot of wasted space.

> OPTIMIZE TABLE copies the data part of the table and rebuilds the
> indexes. The benefits come from improved packing of data within
> indexes, and reduced fragmentation within the tablespaces and on disk.
> The benefits vary depending on the data in each table. You may find
> that there are significant gains for some and not for others, or that
> the gains decrease over time until you next optimize the table. This
> operation can be slow if the table is large or if the indexes being
> rebuilt do not fit into the buffer pool. The first run after adding a
> lot of data to a table is often much slower than later runs.

## Building a composite index
In many situations, a composite index performs better than an index with a single column.  To build an optimal composite index, populate it with columns in this order.

* `=` column(s) from the `WHERE` clause first.  (eg, `INDEX(a,b,...)` for `WHERE a=12 AND b='xyz' ...`)
* `IN` column(s); the optimizer may be able to leapfrog through the index.
* One "range"  (eg `x BETWEEN 3 AND 9`, `name LIKE 'J%'`) It won't use anything past the first range column.
* All the columns in `GROUP BY`, in order
* All the columns in `ORDER BY`, in order.  Works only if all are `ASC` or all are `DESC` or you are using 8.0.

Notes and exceptions:

* Don't duplicate any columns.
* Skip over any cases that don't apply.
* If you don't use all the columns of `WHERE`, there is no need to go on to `GROUP BY`, etc.
* There are cases where it is useful to index only the `ORDER BY` column(s), ignoring `WHERE`.
* Don't "hide" a column in a function (eg `DATE(x) = ...` cannot use `x` in the index.)
* 'Prefix' indexing (eg, `text_col(99)`) is unlikely to be helpful; may hurt.

[_More details and tips_](https://mariadb.com/kb/en/mariadb/compound-composite-indexes/) .

## Select Statement Optimization
Below are some tips to remember while we are writing a select query in MySQL that can help us and reduce our query time:-
1. Whenever we use where in a large table we should make sure the column in where clause are index or not.
Ex:- Select * from employee where user_id > 2000.
user_id if indexed then will speed up the evaluation of the query atlot. Indexes are also very important during joins and foreign keys.

2. When you need the smaller section of content rather then fetching whole data from table, try to use limit.
Rather then writing 
Ex:- Select * from employee.
If you need just first 20 employee from lakhs then just use limit
Ex:- Select * from employee LIMIT 20.

3. You can also optimize your query by providing the column name which you want in resultset.
Rather then writing 
Ex:- Select * from employee.
Just mention column name from which you need data if you table has lots of column and you want to have data for few of them.
Ex:- Select id, name from employee.

4. Index column if you are using to verify for NULL in where clause.
If you have some statement as SELECT * FROM tbl_name WHERE key_col IS NULL;
then if key_col is indexed then query will be evaluated faster.



