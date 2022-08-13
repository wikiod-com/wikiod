---
title: "Sequences"
slug: "sequences"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Create Sequence
    CREATE SEQUENCE [dbo].[CustomersSeq]
    AS INT
    START WITH 10001
    INCREMENT BY 1
    MINVALUE -1;


## Use Sequence in Table
    CREATE TABLE [dbo].[Customers]
    (
        CustomerID INT DEFAULT (NEXT VALUE FOR [dbo].[CustomersSeq]) NOT NULL,
        CustomerName VARCHAR(100),
    );



## Insert Into Table with Sequence
    INSERT INTO [dbo].[Customers]
           ([CustomerName])
     VALUES
           ('Jerry'),
           ('Gorge')

    SELECT * FROM [dbo].[Customers]

**Results**

| CustomerID | CustomerName|
| ------ | ------ |
| 10001| Jerry|
| 10002| Gorge|



## Delete From & Insert New
    DELETE FROM [dbo].[Customers]
    WHERE CustomerName = 'Gorge';

    INSERT INTO [dbo].[Customers]
           ([CustomerName])
     VALUES ('George')

    SELECT * FROM [dbo].[Customers]

**Results**

| CustomerID | CustomerName|
| ------ | ------ |
| 10001| Jerry|
| 10003| George|



