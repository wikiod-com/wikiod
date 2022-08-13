---
title: "PREPARE Statements"
slug: "prepare-statements"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Syntax
 - PREPARE stmt_name FROM preparable_stmt
 - EXECUTE stmt_name [USING @var_name [, @var_name] ...]
 - {DEALLOCATE | DROP} PREPARE stmt_name

## PREPARE, EXECUTE and DEALLOCATE PREPARE Statements
[PREPARE][1] prepares a statement for execution

[EXECUTE][2] executes a prepared statement
    
[DEALLOCATE PREPARE][3] releases a prepared statement
   
    
    SET @s = 'SELECT SQRT(POW(?,2) + POW(?,2)) AS hypotenuse';
    PREPARE stmt2 FROM @s;
    SET @a = 6;
    SET @b = 8;
    EXECUTE stmt2 USING @a, @b;

Result:

    +------------+
    | hypotenuse |
    +------------+
    |         10 |
    +------------+

Finally,

    DEALLOCATE PREPARE stmt2;

Notes:

* You must use @variables, not DECLAREd variables for `FROM @s`
* A primary use for Prepare, etc, is to 'construct' a query for situations where binding will not work, such as inserting the table name.


  [1]: http://dev.mysql.com/doc/refman/5.7/en/prepare.html
  [2]: http://dev.mysql.com/doc/refman/5.7/en/execute.html
  [3]: http://dev.mysql.com/doc/refman/5.7/en/deallocate-prepare.html

## Alter table with add column
    SET v_column_definition := CONCAT(
      v_column_name
      ,' ',v_column_type
      ,' ',v_column_options 
    );
    
    SET @stmt := CONCAT('ALTER TABLE ADD COLUMN ', v_column_definition);
    
    PREPARE stmt FROM @stmt;
    EXECUTE stmt;
    DEALLOCATE PREPARE stmt;


## Construct and execute
(This is a request for a good example that shows how to _construct_ a `SELECT` using `CONCAT`, then prepare+execute it.  Please emphasize the use of @variables versus DECLAREd variables -- it makes a big difference, and it is something that novices (include myself) stumble over.)

