---
title : oozie Tutorial
slug : oozie-tutorial
weight : 9981
draft : false
images : []
type : docs
---

Oozie is an Apache open source project, originally developed at Yahoo. Oozie is a general purpose scheduling system for multistage Hadoop jobs. 

 - Oozie allow to form a logical grouping of relevant Hadoop jobs into an entity called `Workflow`. The Oozie workflows are DAG (Directed cyclic graph) of actions. 
 - Oozie provides a way to schedule **Time** or **Data** dependent Workflow using an entity called `Coordinator`. 
 - Further you can combine the related Coordinators into an entity called `Bundle` and can be scheduled on a Oozie server for execution.

Oozie support most of the Hadoop Jobs as Oozie Action Nodes like: `MapRedude`, `Java`, `FileSystem` (HDFS operations), `Hive`, `Hive2`, `Pig`, `Spark`, `SSH`, `Shell`, `DistCp` and `Sqoop`. It provides a decision capability using a `Decision Control Node` action and Parallel execution of the jobs using `Fork-Join Control Node`. It allow users to configure email option for Success/Failure notification of the Workflow using `Email` action.

