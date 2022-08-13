---
title: "Parse comma-separated values in a column into multiple rows"
slug: "parse-comma-separated-values-in-a-column-into-multiple-rows"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
- WITH *CTE_name* (*column_name*[,...]) AS (
-   SELECT *column_name*[,...] FROM *base_table*
-   UNION ALL
-   SELECT *column_name*[,...] FROM *CTE_name*
-   WHERE <*recursion limiting condition*>
- )
- SELECT *column_name*[,...] FROM *CTE_name*

## Recursive query to parse comma-separated values
Although storing multiple values in a single column violates normalization rules, sometimes one has to deal with badly designed legacy tables. A recursive query can help convert comma-separated values into distinct rows.

Create a sample badly designed table and insert some data:

    create table projects (name varchar(10), members varchar(1000));

    insert into projects (name, members) values ('Luna', '1, 3, 4'), ('Terra', '2,3,5'); 

Check what we have: 

    select * from projects;

will output

    NAME       MEMBERS                                
    ---------- -------------------------
    Luna       1, 3, 4                                                 
    Terra      2,3,5           

    2 record(s) selected.

Use a common table expression (CTE) to recursively extract each comma-separated value from `MEMBERS` into its own row: 

    WITH parse (lvl, name, member, tail) AS (  
      SELECT 1, name,     
             CASE WHEN LOCATE(',',members) > 0 
                  THEN TRIM(LEFT(members, LOCATE(',',members)-1))
                  ELSE TRIM(members) 
             END,    
             CASE WHEN LOCATE(',',members) > 0 
                  THEN SUBSTR(members, LOCATE(',',members)+1)    
                  ELSE '' 
             END  
      FROM projects 
      UNION ALL  
      SELECT lvl + 1, name,      
             CASE WHEN LOCATE(',', tail) > 0 
                  THEN TRIM(LEFT(tail, LOCATE(',', tail)-1))    
                  ELSE TRIM(tail) 
             END,    
             CASE WHEN LOCATE(',', tail) > 0 
                  THEN SUBSTR(tail, LOCATE(',', tail)+1)    
                  ELSE '' 
             END
      FROM parse 
      WHERE lvl < 100 AND tail != '')
      SELECT name, integer(member) member FROM parse
      ORDER BY 1

will return

    NAME       MEMBER     
    ---------- -----------
    Luna                 1
    Luna                 3
    Luna                 4
    Terra                2
    Terra                3
    Terra                5
    
      6 record(s) selected. 

The result returned by the CTE can be used as a regular table, e.g. by joining it to another table. For example, create an employee lookup table:

    create table employees (id integer, name varchar(20));
    insert into employees (id, name) values (1, 'John'), (2, 'Peter'), 
                                            (3, 'Venkat'), (4, 'Mishka'), (5, 'Xiao');

Then the following query

    WITH parse (lvl, name, member, tail) AS (  
      SELECT 1, name,     
             CASE WHEN LOCATE(',',members) > 0 
                  THEN TRIM(LEFT(members, LOCATE(',',members)-1))
                  ELSE TRIM(members) 
             END,    
             CASE WHEN LOCATE(',',members) > 0 
                  THEN SUBSTR(members, LOCATE(',',members)+1)    
                  ELSE '' 
             END  
      FROM projects 
      UNION ALL  
      SELECT lvl + 1, name,      
             CASE WHEN LOCATE(',', tail) > 0 
                  THEN TRIM(LEFT(tail, LOCATE(',', tail)-1))    
                  ELSE TRIM(tail) 
             END,    
             CASE WHEN LOCATE(',', tail) > 0 
                  THEN SUBSTR(tail, LOCATE(',', tail)+1)    
                  ELSE '' 
             END
      FROM parse 
      WHERE lvl < 100 AND tail != '')
      SELECT p.name "Project name", e.name "Member name" 
      FROM parse p
      INNER JOIN employees e
      ON e.id = integer(p.member)
      ORDER BY 1, 2

will return

    Project name Member name         
    ------------ --------------------
    Luna         John                
    Luna         Mishka              
    Luna         Venkat              
    Terra        Peter               
    Terra        Venkat              
    Terra        Xiao                
    
      6 record(s) selected.

