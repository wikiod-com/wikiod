---
title: "Table Creation Script with sample data"
slug: "table-creation-script-with-sample-data"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Date and Timestamp types
    CREATE TABLE all_datetime_types(
      c_date date, 
      c_timestamp timestamp
    );

Minimum and maximum data values:


    insert into all_datetime_types values ('0001-01-01','0001-01-01 00:00:00.000000001');
    insert into all_datetime_types values ('9999-12-31','9999-12-31 23:59:59.999999999');



## Text Types
    CREATE TABLE all_text_types(
      c_char char(255), 
      c_varchar varchar(65535), 
      c_string string 
    );

Sample data:

    insert into all_text_type values ('some ****&&&%%% char value ','some $$$$#####@@@@ varchar value','some !!~~~++ string value' );

## Numeric Types
    CREATE TABLE all_numeric_types(
      c_tinyint tinyint, 
      c_smallint smallint, 
      c_int int, 
      c_bigint bigint, 
      c_decimal decimal(38,3)
    );

Minimum and maximum data values:

    insert into all_numeric_types values (-128,-32768,-2147483648,-9223372036854775808,-99999999999999999999999999999999999.999);
    insert into all_numeric_types values (127,32767,2147483647,9223372036854775807,99999999999999999999999999999999999.999);

## Floating Point Numeric Types
    CREATE TABLE all_floating_numeric_types(
      c_float float, 
      c_double double
    );

Minimum and maximum data values:

    insert into all_floating_numeric_types values (-3.4028235E38,-1.7976931348623157E308);
    insert into all_floating_numeric_types values (-1.4E-45,-4.9E-324);
    insert into all_floating_numeric_types values (1.4E-45,4.9E-324);
    insert into all_floating_numeric_types values (3.4028235E38,1.7976931348623157E308);



## Boolean and Binary Types 
    CREATE TABLE all_binary_types(
      c_boolean boolean, 
      c_binary binary 
    );

Sample data:

    insert into all_binary_types values (0,1234);
    insert into all_binary_types values (1,4321);


Note:
-----

- For boolean, internally it stored as true or false.
- For binary, it will store base64 encoded value.



## Complex Types

ARRAY
=====


    CREATE TABLE array_data_type(
      c_array array<string>)
     ROW FORMAT DELIMITED 
      FIELDS TERMINATED BY ',' 
      COLLECTION ITEMS TERMINATED BY '&';

Create `data.csv` with data:

    arr1&arr2
    arr2&arr4


Put `data.csv` in `/tmp` folderand load this data in Hive


    LOAD DATA LOCAL INPATH '/tmp/data.csv' INTO TABLE array_data_type;


Or you can put this CSV in HDFS say at `/tmp`. Load data from CSV at HDFS using 


    LOAD DATA INPATH '/tmp/data.csv' INTO TABLE array_data_type;

-----------------------------------------------------------
MAP
===

    CREATE TABLE map_data_type(
      c_map map<int,string>)
     ROW FORMAT DELIMITED 
      FIELDS TERMINATED BY ',' 
      COLLECTION ITEMS TERMINATED BY '&'
      MAP KEYS TERMINATED BY '#';


`data.csv` file:

    101#map1&102#map2
    103#map3&104#map4

Load data into hive:

    LOAD DATA LOCAL INPATH '/tmp/data.csv' INTO TABLE map_data_type;

-------------------------------------------------------------------

STRUCT
=====

    CREATE TABLE struct_data_type(
      c_struct struct<c1:smallint,c2:varchar(30)>)
     ROW FORMAT DELIMITED 
      FIELDS TERMINATED BY ',' 
      COLLECTION ITEMS TERMINATED BY '&';


`data.csv` file:

    101&struct1
    102&struct2

Load data into hive:

    LOAD DATA LOCAL INPATH '/tmp/data.csv' INTO TABLE struct_data_type;

----------------------------------------------------------------------

UNIONTYPE
=========

    CREATE TABLE uniontype_data_type(
      c_uniontype uniontype<int, double, array<string>)
     ROW FORMAT DELIMITED 
      FIELDS TERMINATED BY ',' 
      COLLECTION ITEMS TERMINATED BY '&';


`data.csv` file:

    0&10
    1&10.23
    2&arr1&arr2


Load data into hive:

    LOAD DATA LOCAL INPATH '/tmp/data.csv' INTO TABLE uniontype_data_type;

-------------------------------------------------------------------------



