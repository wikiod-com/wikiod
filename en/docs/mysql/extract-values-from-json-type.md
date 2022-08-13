---
title: "Extract values from JSON type"
slug: "extract-values-from-json-type"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

MySQL 5.7.8+ supports native JSON type. While you have different ways to create json objects, you can access and read members in different ways, too.

Main function is `JSON_EXTRACT`, hence `->` and `->>` operators are more friendly.

## Syntax
- JSON_EXTRACT(json_doc,path[,...])
- JSON_EXTRACT(json_doc,path)
- JSON_EXTRACT(json_doc,path1,path2)

## Parameters
Parameter|Description
------|------
json_doc|valid JSON document
path|members path

Mentioned in [MySQL 5.7 Reference Manual](https://dev.mysql.com/doc/refman/5.7/en/json-search-functions.html#function_json-extract)
 - Multiple matched values by path argument(s)
> If it is possible that those arguments could return multiple values, the matched values are autowrapped as an array, in the order corresponding to the paths that produced them. Otherwise, the return value is the single matched value.

- `NULL` Result when:
  - any argemunt is NULL
  - path not matched

> Returns NULL if any argument is NULL or no paths locate a value in the document.

## Read JSON Array value
Create @myjson variable as JSON type ([read more](https://www.wikiod.com/mysql/json)):

     SET @myjson = CAST('["A","B",{"id":1,"label":"C"}]' as JSON) ;

`SELECT` some members!

     SELECT
       JSON_EXTRACT( @myjson , '$[1]' ) ,
       JSON_EXTRACT( @myjson , '$[*].label') ,
       JSON_EXTRACT( @myjson , '$[1].*' ) ,
       JSON_EXTRACT( @myjson , '$[2].*')
     ;
     -- result values:
     '\"B\"', '[\"C\"]', NULL, '[1, \"C\"]'
     -- visually:
     "B", ["C"], NULL, [1, "C"]


## JSON Extract Operators
Extract `path` by `->` or `->>` Operators, while `->>` is UNQUOTED value:

     SELECT
       myjson_col->>'$[1]' , myjson_col->'$[1]' ,
       myjson_col->>'$[*].label' ,
       myjson_col->>'$[1].*' ,
       myjson_col->>'$[2].*'
     FROM tablename ;
      -- visuall:
         B, "B" , ["C"], NULL, [1, "C"]
      --^^^ ^^^
So `col->>path` is equal to `JSON_UNQUOTE(JSON_EXTRACT(col,path))` :
> As with ->, the ->> operator is always expanded in the output of EXPLAIN, as the following example demonstrates:
>
>     mysql> EXPLAIN SELECT c->>'$.name' AS name   
>         ->     FROM jemp WHERE g > 2\G
>     *************************** 1. row ***************************
>                id: 1
>       select_type: SIMPLE
>             table: jemp
>        partitions: NULL
>              type: range
>     possible_keys: i
>               key: i
>           key_len: 5
>               ref: NULL
>              rows: 2
>          filtered: 100.00
>             Extra: Using where
>     1 row in set, 1 warning (0.00 sec)
>     
>     mysql> SHOW WARNINGS\G
>     *************************** 1. row ***************************
>       Level: Note
>        Code: 1003
>     Message: /* select#1 */ select
>     json_unquote(json_extract(`jtest`.`jemp`.`c`,'$.name')) AS `name` from
>     `jtest`.`jemp` where (`jtest`.`jemp`.`g` > 2)
>     1 row in set (0.00 sec)
> Read about [inline path extract(+)](https://dev.mysql.com/doc/refman/5.7/en/json-search-functions.html#operator_json-inline-path)

