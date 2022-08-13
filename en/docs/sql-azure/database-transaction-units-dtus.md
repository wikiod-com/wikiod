---
title: "Database Transaction Units (DTUs)"
slug: "database-transaction-units-dtus"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Database Transaction Units (DTU) are the unit in which the performance of Azure SQL DB is measured. DTUs make the most sense when used for comparing workloads. For example, a workload that uses 5 DTUs will consume 10 DTUs when doubled.

Officialy, Microsoft has introduced the DTU as the average number of realistic transactions that can be executed per second. Investigating workloads shows that the percentage of DTU used is defined as the MAX of the percentage of CPU, Log I/O and Data I/O used.

It seems that DTU's are not completly comparable between service layers. When moving a workload of 95 DTU from a S3 database (100 DTU) to a P1 database (125 DUT) one can expect to see the average load on the P1 to drop below the 95 DTU.

There is no official public statement from Microsoft on why they choose to use DTU's as the measure of performance/scaling for Azure SQL DB. However, this is in line with other services like Stream Analytics and the Event Hub who's performance is also measured in custom metrics, e.g. Streaming Units and Throughput Units.

There are also eDTUs, which are related to the use of Elastic Pools. Performance wise a DTU and eDTU are the same.

## Investigating where your DTU's go
[![DTU usage, compared to other metics][1]][1]


  [1]: https://i.stack.imgur.com/WsOlt.png

If you want to investigate why you are maxing out your DTUs, add other metrics to the performance graph to see which metric is dominiationg your DTU usage. In this example, the DTU usage is clearly dominiated by the CPU usage

