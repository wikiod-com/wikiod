---
title: "File formats in HIVE"
slug: "file-formats-in-hive"
draft: false
images: []
weight: 9711
type: docs
toc: true
---

## PARQUET
Parquet columnar storage format in Hive 0.13.0 and later. Parquet is built from the ground up with complex nested data structures in mind, and uses the record shredding and assembly algorithm described in the Dremel paper. We believe this approach is superior to simple flattening of nested name spaces.

Parquet is built to support very efficient compression and encoding schemes. Multiple projects have demonstrated the performance impact of applying the right compression and encoding scheme to the data. Parquet allows compression schemes to be specified on a per-column level, and is future-proofed to allow adding more encodings as they are invented and implemented.

Parquet is recommended File Format with Impala Tables in Cloudera distributions.

See: http://parquet.apache.org/documentation/latest/

    CREATE TABLE parquet_table_name (x INT, y STRING) STORED AS PARQUET;

## SEQUENCEFILE 
Store data in SEQUENCEFILE if the data needs to be compressed. You can import text files compressed with Gzip or Bzip2 directly into a table stored as TextFile. The compression will be detected automatically and the file will be decompressed on-the-fly during query execution. 

    CREATE TABLE raw_sequence (line STRING)
    STORED AS SEQUENCEFILE;

## ORC
The Optimized Row Columnar (ORC) file format provides a highly efficient way to store Hive data. It was designed to overcome limitations of the other Hive file formats. Using ORC files improves performance when Hive is reading, writing, and processing data. ORC file can contain lightweight indexes and bloom filters.

See: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+ORC

ORC is a recommended format for storing data within HortonWorks distribution.
 

    CREATE TABLE tab_orc (col1 STRING,
                          col2 STRING,
                          col3 STRING)
    STORED AS ORC
    TBLPROPERTIES (
                   "orc.compress"="SNAPPY",
                   "orc.bloom.filter.columns"="col1",
                   "orc.create.index" = "true" 
                  ) 

To modify a table so that new partitions of the table are stored as ORC files:

    ALTER TABLE T SET FILEFORMAT ORC; 

As of Hive 0.14, users can request an efficient merge of small ORC files together by issuing a `CONCATENATE` command on their table or partition. The files will be merged at the stripe level without reserializatoin.

    ALTER TABLE T [PARTITION partition_spec] CONCATENATE;

## AVRO
Avro files are been supported in Hive 0.14.0 and later. 

Avro is a remote procedure call and data serialization framework developed within Apache's Hadoop project. It uses JSON for defining data types and protocols, and serializes data in a compact binary format. Its primary use is in Apache Hadoop, where it can provide both a serialization format for persistent data, and a wire format for communication between Hadoop nodes, and from client programs to the Hadoop services.

Specification of AVRO format: https://avro.apache.org/docs/1.7.7/spec.html

    CREATE TABLE kst
    PARTITIONED BY (ds string)
    STORED AS AVRO
    TBLPROPERTIES (
      'avro.schema.url'='http://schema_provider/kst.avsc');
    
We can also use below syntax without using schema file.

    CREATE TABLE kst (field1 string, field2 int)
    PARTITIONED BY (ds string)
    STORED AS AVRO;

In the examples above `STORED AS AVRO` clause is equivalent to:

    ROW FORMAT SERDE
      'org.apache.hadoop.hive.serde2.avro.AvroSerDe'
    STORED AS INPUTFORMAT
      'org.apache.hadoop.hive.ql.io.avro.AvroContainerInputFormat'
    OUTPUTFORMAT
      'org.apache.hadoop.hive.ql.io.avro.AvroContainerOutputFormat'


## Text File
TextFile is the default file format, unless the configuration parameter hive.default.fileformat has a different setting.
We can create a table on hive using the field names in our delimited text file. Lets say for example, our csv file contains three fields (id, name, salary) and we want to create a table in hive called "employees". We will use the below code to create the table in hive.     
`CREATE TABLE employees (id int, name string, salary double)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ‘,’;`

Now we can load a text file into our table:

`LOAD DATA LOCAL INPATH '/home/ourcsvfile.csv' OVERWRITE INTO TABLE employees;`

Displaying the contents of our table on hive to check if the data was successfully loaded:

`SELECT * FROM employees;`

