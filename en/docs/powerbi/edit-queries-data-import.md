---
title: "Edit Queries (data import)"
slug: "edit-queries-data-import"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This technique can also be used for other data sources e.g. files, web services.

Note that when you do change these settings, you may have to specify Authentication/Credential details for the new source.

## Preventing data source string duplication
By default, when data is imported to the PowerBI Desktop, each table or query stores data source details separately, even if they use the same data source.

This makes it tedious, for example, to change the source database of an entire PowerBI report - which requires changing each query source parameter individually.

There is a way to simplify this, described [here](https://blog.crossjoin.co.uk/2015/11/09/avoiding-duplication-of-database-connection-information-in-power-bi/).

---

1) Import your data as you normally would. Example:  
data source type - SQL server,  
server name - `localhost`,  
database - `test1`,  
tables - `table1`, `table2`.  

2) In query editor, add two blank queries: "Get Data" -> "Blank Query",  
`serverName` with value `= "localhost"`,  
`databaseName` with value `= "test1"`.  
[![enter image description here][1]][1]  

3) For each table on the "Queries" pane on the left, select "Source" in "Query Settings" on the right, then substitute server and database names with parameters created in the step 2.  
[![enter image description here][2]][2]  
[![enter image description here][4]][4]
[![enter image description here][5]][5]  

4) Now when you need to update your connection string, change `serverName` or `databaseName` and refresh data.

  [1]: http://i.stack.imgur.com/tUB9X.png
  [2]: http://i.stack.imgur.com/KgshE.png
  [3]: http://i.stack.imgur.com/Nv6Cv.png
  [4]: http://i.stack.imgur.com/7kWdI.png
  [5]: http://i.stack.imgur.com/rUiqe.png

