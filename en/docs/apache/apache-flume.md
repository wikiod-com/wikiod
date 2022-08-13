---
title: "Apache Flume"
slug: "apache-flume"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

`Apache Flume` is a tool/service/data ingestion mechanism for collecting aggregating and transporting large amounts of streaming data such as log files, events (etc...) from various sources to a **centralized data store**.

Flume is a highly reliable, distributed, and configurable tool. It is principally designed to copy streaming data (log data) from various web servers to `HDFS`.

## Streaming / Log Data
Generally, most of the data that is to be analyzed will be produced by various data sources like applications servers, social networking sites, cloud servers, and enterprise servers. This data will be in the form of log files and events.

Log file − In general, a log file is a file that lists events/actions that occur in an operating system. For example, web servers list every request made to the server in the log files.

On harvesting such log data, we can get information about −

the application performance and locate various software and hardware failures.
the user behavior and derive better business insights.
The traditional method of transferring data into the HDFS system is to use the put command. Let us see how to use the put command.

