---
title: "Working with databases"
slug: "working-with-databases"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Simple use of PostgreSQL with Postmodern
[Postmodern](http://marijnhaverbeke.nl/postmodern/) is a library to interface the relational database [PostgreSQL](https://postgresql.org). It offers several levels of access to PostgreSQL, from the execution of SQL queries represented as strings, or as lists, to an object-relational mapping.

The database used in the following examples can be created with these SQL statements:

    create table employees
      (empid integer not null primary key,
       name text not null,
       birthdate date not null,
       skills text[] not null);
    insert into employees (empid, name, birthdate, skills) values
      (1, 'John Orange', '1991-07-26', '{C, Java}'),
      (2, 'Mary Red', '1989-04-14', '{C, Common Lisp, Hunchentoot}'),
      (3, 'Ron Blue', '1974-01-17', '{JavaScript, Common Lisp}'),
      (4, 'Lucy Green', '1968-02-02', '{Java, JavaScript}');

The first example shows the result of a simple query returning a relation:

    CL-USER> (ql:quickload "postmodern")   ; load the system postmodern (nickname: pomo)
    ("postmodern")
    CL-USER> (let ((parameters '("database" "dbuser" "dbpass" "localhost")))
               (pomo:with-connection parameters
                 (pomo:query "select name, skills from employees")))
    (("John Orange" #("C" "Java"))                  ; output manually edited!
     ("Mary Red" #("C" "Common Lisp" "Hunchentoot"))
     ("Ron Blue" #("JavaScript" "Common Lisp"))
     ("Lucy Green" #("Java" "JavaScript")))
    4                                               ; the second value is the size of the result

Note that the result can be returned as list of alists or plists adding the optional parameters `:alists` or `:plists` to the query function.

An alternative to `query` is `doquery`, to iterate over the results of a query. Its parameters are `query (&rest names) &body body`, where names are bound to the values in the row at each iteration: 

    CL-USER> (let ((parameters '("database" "dbuser" "dbpass" "localhost")))
               (pomo:with-connection parameters
                 (format t "The employees that knows Java are:~%")
                 (pomo:doquery "select empid, name from employees where skills @> '{Java}'" (i n)
                   (format t "~a (id = ~a)~%" n i))))
    The employees that knows Java are:
    John Orange (id = 1)
    Lucy Green (id = 4)
    NIL
    2

When the query requires parameters, one can use prepared statements:

    CL-USER> (let ((parameters '("database" "dbuser" "dbpass" "localhost")))
               (pomo:with-connection parameters
                 (funcall
                   (pomo:prepare "select name, skills from employees where skills @> $1")
                   #("Common Lisp"))))    ; find employees with skills including Common Lisp
    (("Mary Red" #("C" "Common Lisp" "Hunchentoot"))
     ("Ron Blue" #("JavaScript" "Common Lisp")))
    2

The function `prepare` receives a query with placeholders `$1`, `$2`, etc. and returns a new function that requires one parameter for each placeholder and executes the query when called with the right number of arguments.

In case of updates, the function `exec` returns the number of tuples modified (the two DDL statements are enclosed in a transaction):

    CL-USER> (let ((parameters '("database" "dbuser" "dbpass" "localhost")))
               (pomo:with-connection parameters
                 (pomo:ensure-transaction
                   (values
                     (pomo:execute "alter table employees add column salary integer")
                     (pomo:execute "update employees set salary =
                                         case when skills @> '{Common Lisp}'
                                         then 100000 else 50000 end")))))
    0
    4

In addition to writing SQL queries as strings, one can use of lists of keywords, symbols and constants, with a syntax reminiscent of lisp (S-SQL):

    CL-USER> (let ((parameters '("database" "dbuser" "dbpass" "localhost")))
               (pomo:with-connection parameters
                 (pomo:query (:select 'name :from 'employees :where (:> 'salary 60000)))))
    (("Mary Red") ("Ron Blue"))
    2






