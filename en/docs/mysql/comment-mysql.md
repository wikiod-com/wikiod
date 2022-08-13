---
title: "Comment Mysql"
slug: "comment-mysql"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

The `--` style of comment, which requires a trailing space, [differs in behavior from the SQL standard][1], which does not require the space.


  [1]: https://dev.mysql.com/doc/mysql-reslimits-excerpt/5.5/en/ansi-diff-comments.html

## Adding comments
There are three types of comment:

     # This comment continues to the end of line

    -- This comment continues to the end of line

    /* This is an in-line comment */ 

    /*
    This is a
    multiple-line comment
    */

Example:

    SELECT * FROM t1; -- this is comment

    CREATE TABLE stack(
        /*id_user int,
        username varchar(30),
        password varchar(30)
        */
        id int
    );

The `--` method requires that a space follows the `--` before the comment begins, otherwise it will be interpreted as a command and usually cause an error.

    #This comment works
    /*This comment works.*/
    --This comment does not.

## Commenting table definitions
    CREATE TABLE menagerie.bird (
        bird_id INT NOT NULL AUTO_INCREMENT,
        species VARCHAR(300) DEFAULT NULL COMMENT 'You can include genus, but never subspecies.',
        INDEX idx_species (species) COMMENT 'We must search on species often.',
        PRIMARY KEY (bird_id)
    ) ENGINE=InnoDB COMMENT 'This table was inaugurated on February 10th.';

Using an `=` after `COMMENT` is optional. ([Official docs][1])

These comments, unlike the others, are saved with the schema and can be retrieved via `SHOW CREATE TABLE` or from `information_schema`.


  [1]: http://dev.mysql.com/doc/refman/5.7/en/create-table.html

