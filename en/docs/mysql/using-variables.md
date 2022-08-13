---
title: "Using Variables"
slug: "using-variables"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Setting Variables
Here are some ways to set variables:

1) You can set a variable to a specific, string, number, date using SET

    EX: SET @var_string = 'my_var';
        SET @var_num = '2'
        SET @var_date = '2015-07-20';

4) you can set a variable to be the result of a select statement using :=

    EX: Select @var := '123';
    (Note: You need to use := when assigning a variable not using the SET syntax, because in other statements, (select, update...) the "=" is used to compare, so when you add a colon before the "=", you are saying "This is not a comparison, this is a SET".)

3) You can set a variable to be the result of a select statement using INTO

    (This was particularly helpful when I needed to dynamically choose which Partitions         to query from)

 EX: SET @start_date = '2015-07-20';
     SET @end_date = '2016-01-31';
    
    #this gets the year month value to use as the partition names
    SET @start_yearmonth = (SELECT EXTRACT(YEAR_MONTH FROM @start_date));
    SET @end_yearmonth = (SELECT EXTRACT(YEAR_MONTH FROM @end_date));
    
    #put the partitions into a variable
    SELECT GROUP_CONCAT(partition_name)
    FROM information_schema.partitions p 
    WHERE table_name = 'partitioned_table'
    AND SUBSTRING_INDEX(partition_name,'P',-1) BETWEEN @start_yearmonth AND @end_yearmonth
    INTO @partitions;
    
    #put the query in a variable. You need to do this, because mysql did not recognize my variable as a variable in that position. You need to concat the value of the variable together with the rest of the query and then execute it as a stmt.
    SET @query =
    CONCAT('CREATE TABLE part_of_partitioned_table (PRIMARY KEY(id))
    SELECT partitioned_table.*
    FROM partitioned_table PARTITION(', @partitions,')
    JOIN users u USING(user_id)
    WHERE date(partitioned_table.date) BETWEEN ', @start_date,' AND ', @end_date);
    
    #prepare the statement from @query
    PREPARE stmt FROM @query;
    #drop table
    DROP TABLE IF EXISTS tech.part_of_partitioned_table;
    #create table using statement
    EXECUTE stmt;
    

## Row Number and Group By using variables in Select Statement
Let's say we have a table `team_person` as below:

    +======+===========+
    | team |    person |
    +======+===========+
    |   A  |      John |
    +------+-----------+
    |   B  |     Smith |
    +------+-----------+
    |   A  |    Walter |
    +------+-----------+
    |   A  |     Louis |
    +------+-----------+
    |   C  | Elizabeth |
    +------+-----------+
    |   B  |     Wayne |
    +------+-----------+


    CREATE TABLE team_person AS SELECT 'A' team, 'John' person
    UNION ALL  SELECT 'B' team,  'Smith' person
    UNION ALL  SELECT 'A' team,  'Walter' person
    UNION ALL  SELECT 'A' team,  'Louis' person
    UNION ALL  SELECT 'C' team,  'Elizabeth' person
    UNION ALL  SELECT 'B' team,  'Wayne' person;

To select the table `team_person` with additional `row_number` column, either

    SELECT @row_no := @row_no+1 AS row_number, team, person
    FROM team_person, (SELECT @row_no := 0) t;

OR 

    SET @row_no := 0;
    SELECT  @row_no := @row_no + 1 AS row_number, team, person
    FROM team_person;

will output the result below:

    +============+======+===========+
    | row_number | team |    person |
    +============+======+===========+
    |          1 |   A  |      John |
    +------------+------+-----------+
    |          2 |   B  |     Smith |
    +------------+------+-----------+
    |          3 |   A  |    Walter |
    +------------+------+-----------+
    |          4 |   A  |     Louis |
    +------------+------+-----------+
    |          5 |   C  | Elizabeth |
    +------------+------+-----------+
    |          6 |   B  |     Wayne |
    +------------+------+-----------+

Finally, if we want to get the `row_number` group by column `team`

    SELECT @row_no := IF(@prev_val = t.team, @row_no + 1, 1) AS row_number
       ,@prev_val := t.team AS team
       ,t.person  
    FROM team_person t,
      (SELECT @row_no := 0) x,
      (SELECT @prev_val := '') y
    ORDER BY t.team ASC,t.person DESC; 

    +============+======+===========+
    | row_number | team |    person |
    +============+======+===========+
    |          1 |   A  |    Walter |
    +------------+------+-----------+
    |          2 |   A  |     Louis |
    +------------+------+-----------+
    |          3 |   A  |      John |
    +------------+------+-----------+
    |          1 |   B  |     Wayne |
    +------------+------+-----------+
    |          2 |   B  |     Smith |
    +------------+------+-----------+
    |          1 |   C  | Elizabeth |
    +------------+------+-----------+

