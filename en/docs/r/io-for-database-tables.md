---
title: "IO for database tables"
slug: "io-for-database-tables"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

# Specialized packages

- RMySQL
- https://www.wikiod.com/r/rodbc 

## Reading Data from MySQL Databases
# General
Using the package [RMySQL][1] we can easily query MySQL as well as MariaDB databases and store the result in an R dataframe:

    library(RMySQL)
    
    mydb <- dbConnect(MySQL(), user='user', password='password', dbname='dbname',host='127.0.0.1')
    
    queryString <- "SELECT * FROM table1 t1 JOIN table2 t2 on t1.id=t2.id"
    query <- dbSendQuery(mydb, queryString)
    data <- fetch(query, n=-1) # n=-1 to return all results

# Using limits
It is also possible to define a limit, e.g. getting only the first 100,000 rows. In order to do so, just change the SQL query regarding the desired limit. The mentioned package will consider these options. Example:

    queryString <- "SELECT * FROM table1 limit 100000"


[1]: https://cran.r-project.org/web/packages/RMySQL/index.html

## Reading Data from MongoDB Databases
In order to load data from a MongoDB database into an R dataframe, use the library [MongoLite][1]:

    # Use MongoLite library:
    #install.packages("mongolite")
    library(jsonlite)
    library(mongolite)
     
    # Connect to the database and the desired collection as root:
    db <- mongo(collection = "Tweets", db = "TweetCollector", url = "mongodb://USERNAME:PASSWORD@HOSTNAME")
    
    # Read the desired documents i.e. Tweets inside one dataframe:
    documents <- db$find(limit = 100000, skip = 0, fields = '{ "_id" : false, "Text" : true }')

The code connects to the server `HOSTNAME` as `USERNAME` with `PASSWORD`, tries to open the database `TweetCollector` and read the collection `Tweets`. The query tries to read the field i.e. column `Text`.

The results is a dataframe with columns as the yielded data set. In case of this example, the dataframe contains the column `Text`, e.g. `documents$Text`.

  [1]: https://github.com/jeroen/mongolite

