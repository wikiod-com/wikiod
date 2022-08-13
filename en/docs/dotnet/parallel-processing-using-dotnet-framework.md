---
title: "Parallel processing using .Net framework"
slug: "parallel-processing-using-net-framework"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

This Topic is about Multi core programming using Task Parallel Library with .NET framework. The task parallel library allows you to write code which is human readable and adjusts itself with the number of Cores available. So you can be sure that your software would auto-upgrade itself with the upgrading  environment. 

## Parallel Extensions
Parallel extensions have been introduced along with the Task Parallel Library to achieve data Parallelism. Data parallelism refers to scenarios in which the same operation is performed concurrently (that is, in parallel) on elements in a source collection or array. The .NET provides new constructs to achieve data parallelism by using Parallel.For and Parallel.Foreach constructs.

    //Sequential version

    foreach (var item in sourcecollection){

    Process(item);

    }

    // Parallel equivalent

    Parallel.foreach(sourcecollection, item => Process(item));


The above mentioned Parallel.ForEach construct utilizes the multiple cores and thus enhances the performance in the same fashion.  

