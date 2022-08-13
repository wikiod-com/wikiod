---
title: "dashDB"
slug: "dashdb"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

IBM dashDB is a family of SQL databases. It's engine is a blend of DB2, Netezza and the [BLU in-memory engine][1]. Any supported DB2 driver will work; it's a drop-replacement for DB2.

The 3 product lines for dashDB are: (1) IBM dashDB for Transactions: A general-purpose, fully managed cloud SQL database. (2) IBM dashDB for Analytics: A cloud SQL data warehouse. (3) IBM dashDB Local: A local version you can install on your own hardware.


  [1]: https://en.wikipedia.org/wiki/IBM_BLU_Acceleration

# For more information about dashDB, you can try these sources:

## General information:
 - Main website: http://dashdb.com/
 - [dashDB for Transactions Bluemix Page][1]
 - [dashDB for Analytics Bluemix Page][2]

## More Documentation
- [dashDB docs on Bluemix][3]
- [IBM Knowledge Center for dashDB][4]
- [API reference for dashDB for Analytics][5]


  [1]: https://ibm.biz/dashdbtx
  [2]: https://console.ng.bluemix.net/catalog/dashdb
  [3]: https://console.ng.bluemix.net/docs/services/dashDB/dashDB.html
  [4]: http://www.ibm.com/support/knowledgecenter/SS6NHC/com.ibm.swg.im.dashdb.kc.doc/welcome.html
  [5]: https://developer.ibm.com/static/site-id/85/api/dashdb-analytics/

## A basic SQL select- List all rows in a table.
SELECT * FROM MY_TABLE

