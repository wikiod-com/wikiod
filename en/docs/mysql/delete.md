---
title: "DELETE"
slug: "delete"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
- DELETE [ LOW_PRIORITY ] [ QUICK ] [ IGNORE ] FROM table
[WHERE conditions]
[ORDER BY expression [ ASC | DESC ]]
[LIMIT number_rows];   /// Syntax for delete row(s) from single table

## Parameters
|Parameter           |Details                 |
|--------------------|------------------------|
|LOW_PRIORITY        | If `LOW_PRIORITY` is provided, the delete will be delayed until there are no processes reading from the table|
|IGNORE              |If `IGNORE` is provided, all errors encountered during the delete are ignored
|table               |The table from which you are going to delete records
|WHERE conditions    |The conditions that must be met for the records to be deleted.      If no conditions are provided, then all records from the table will be deleted
|ORDER BY expression |If `ORDER BY` is provided, records will be deleted in the given order
|LIMIT               |It controls the maximum number of records to delete from the table. Given `number_rows` will be deleted.


## Delete with Where clause
    DELETE FROM `table_name` WHERE `field_one` = 'value_one'
This will delete all rows from the table where the contents of the `field_one` for that row match 'value_one'

The `WHERE` clause works in the same way as a select, so things like `>`, `<`, `<>` or `LIKE` can be used.

**Notice:** It is necessary to use conditional clauses (WHERE, LIKE) in delete query. If you do not use any conditional clauses then all data from that table will be deleted.

## Delete all rows from a table
    DELETE FROM table_name ;

This will delete everything, all rows from the table. It is the most basic example of the syntax. It also shows that `DELETE` statements should really be used with extra care as they may empty a table, if the `WHERE` clause is omitted.

## LIMITing deletes
    DELETE FROM `table_name` WHERE `field_one` = 'value_one' LIMIT 1
This works in the same way as the 'Delete with Where clause' example, but it will stop the deletion once the limited number of rows have been removed.

If you are limiting rows for deletion like this, be aware that it will delete the first row which matches the criteria. It might not be the one you would expect, as the results can come back unsorted if they are not explicitly ordered.

## Multi-Table Deletes


MySQL's `DELETE` statement can use the `JOIN` construct, allowing also to specify which tables to delete from. This is useful to avoid nested queries.
Given the schema:

    create table people
    (    id int primary key,
        name varchar(100) not null,
        gender char(1) not null
    );
    insert people (id,name,gender) values
    (1,'Kathy','f'),(2,'John','m'),(3,'Paul','m'),(4,'Kim','f');
    
    create table pets
    (    id int auto_increment primary key,
        ownerId int not null,
        name varchar(100) not null,
        color varchar(100) not null
    );
    insert pets(ownerId,name,color) values 
    (1,'Rover','beige'),(2,'Bubbles','purple'),(3,'Spot','black and white'),
    (1,'Rover2','white');



| id | name  | gender |
|----|-------|--------|
|  1 | Kathy | f      |
|  2 | John  | m      |
|  3 | Paul  | m      |
|  4 | Kim   | f      |


| id  | ownerId  | name     | color   |
|-----|----------|----------|---------|
|   1 |        1 | Rover    | beige   |
|   2 |        2 | Bubbles  | purple  |
|   4 |        1 | Rover2   | white   |




If we want to remove Paul's pets, the statement

    DELETE p2
    FROM pets p2
    WHERE p2.ownerId in (
        SELECT p1.id
        FROM people p1
        WHERE p1.name = 'Paul');


can be rewritten as:

    DELETE p2    -- remove only rows from pets
    FROM people p1
    JOIN pets p2
    ON p2.ownerId = p1.id
    WHERE p1.name = 'Paul';

*1 row deleted*<br/>
Spot is deleted from Pets

`p1` and `p2` are aliases for the table names, especially useful for long table names and ease of readability.


To remove both the person and the pet:

    DELETE p1, p2     -- remove rows from both tables
    FROM people p1
    JOIN pets p2
    ON p2.ownerId = p1.id
    WHERE p1.name = 'Paul';

*2 rows deleted*<br/>
Spot is deleted from Pets<br/>
Paul is deleted from People


foreign keys
============

When the DELETE statement involes tables with a foreing key constrain the optimizer may process the tables in an order that does not follow the relationship.
Adding for example a foreign key to the definition of `pets`

    ALTER TABLE pets ADD CONSTRAINT `fk_pets_2_people` FOREIGN KEY (ownerId) references people(id) ON DELETE CASCADE;

 the engine may try to delete the entries from `people` before `pets`, thus causing the following error:

    ERROR 1451 (23000): Cannot delete or update a parent row: a foreign key constraint fails (`test`.`pets`, CONSTRAINT `pets_ibfk_1` FOREIGN KEY (`ownerId`) REFERENCES `people` (`id`))

The solution in this case is to delete the row from `people` and rely on `InnoDB`'s `ON DELETE` capabilities to propagate the deletion:

    DELETE FROM people
    WHERE name = 'Paul';

*2 rows deleted*<br/>
Paul is deleted from People<br/>
Spot is deleted on cascade from Pets

Another solution is to temporarily disable the check on foreing keys:

    SET foreign_key_checks = 0;
    DELETE p1, p2 FROM people p1 JOIN pets p2 ON p2.ownerId = p1.id WHERE p1.name = 'Paul';
    SET foreign_key_checks = 1;


## Basic delete


## DELETE vs TRUNCATE
    TRUNCATE tableName;
This will [delete][1] all the data and reset `AUTO_INCREMENT` index. It's much faster than `DELETE FROM tableName` on a huge dataset. It can be very useful during development/testing.

 When you ***truncate*** a table SQL server doesn't delete the data, it drops the table and recreates it, thereby deallocating the pages so there is a chance to recover the truncated data before the pages where overwritten.  (The space cannot immediately be recouped for `innodb_file_per_table=OFF`.)  


  [1]: http://stackoverflow.com/a/30997025/5006740

## Multi-table DELETE


