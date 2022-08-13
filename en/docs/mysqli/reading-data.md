---
title: "Reading data"
slug: "reading-data"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Read the data that is collected.
$result represents the result from the query.

## Syntax
 1. $result->mysqli_fetch_array(MYSQLI_BOTH)
 2. $result->mysqli_fetch_array(MYSQLI_NUM)

## Parameters
| Parameter | Description|
| ------ | ------ |
| MYSQLI_BOTH | tells the script to use the column names as indexes e.g. $data['username']|
| MYSQLI_NUM | tells the script to use number indexes e.g. $data[1] |

## MYSQLi result into a numbered array
    $data = $result->mysqli_fetch_array(MYSQLI_NUM);

## MYSQLi data into a column name array
    $data = $result->mysqli_fetch_array(MYSQLI_BOTH);

