---
title: "Getting started with google-bigquery"
slug: "getting-started-with-google-bigquery"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Analyzing 50 billion Wikipedia pageviews in 5 seconds (BigQuery beginner tutorial)
Hi everyone! This is a demo I love running for people that get started with BigQuery. So let's run some simple queries to get **you** started.

Setup
-----

You will need a Google Cloud project:

1. Go to http://bigquery.cloud.google.com/.
2. If it tells you to create a project, follow the link to create a project, and create a project.
3. Come back to http://bigquery.cloud.google.com/.

Notes:

* You don't need a credit card. Everyone gets a free 1TB for analysis each month.
* BigQuery charges per query. Before running a query you will be able to see how much each query costs.


Let's query
----

1. Find the pageviews for May 2015 at https://bigquery.cloud.google.com/table/fh-bigquery:wikipedia.pagecounts_201505

  *Note*: Google protects your data with the highest security standards (PCI, ISO, HIPAA, SOC, etc), but it's also easy to share data if you want so - as I did here. https://cloud.google.com/security/

2. This table has 5 columns: *datehour     language     title     requests     content_size*. They basically say "this wikipedia page in this language had these many requests at this hour".
3. This table has almost 6 billion rows (379 GB of data).
4. To find out how many pageviews Wikipedia had during May, you can add up all the 6 billion lines of requests:

        SELECT SUM(requests) 
        FROM [fh-bigquery:wikipedia.pagecounts_201505]

5. Did you notice how fast that was? (1.8s elapsed, 43.1 GB processed for me)
6. Let's do something more complex. Let's run a regular expression over these 6 billion rows. How fast could this be?
        
        SELECT SUM(requests) req, title
        FROM [fh-bigquery:wikipedia.pagecounts_201505] 
        WHERE REGEXP_MATCH(title, 'Red.*t')
        GROUP BY title
        ORDER BY req DESC
        LIMIT 100     

7. How fast was it for you? Did you find Reddit in the results?

Cost analysis
-----
1. This last query processed 269 GB: More than a quarter of the free monthly terabyte. Why?
2. BigQuery looks at the columns you process on your query. 'title' is a big column - it contains text. The 'requests' column is only 43.1 GB.
3. To make your free terabyte last, extract data to smaller tables. For example, I have a table with only the [top 65,000 English Wikipedia pages pageviews](https://bigquery.cloud.google.com/table/fh-bigquery:wikipedia.pagecounts_201408_en_top65k). The same query processes only 1.18 GB - you can run almost a 1000 of them for free a month.
        
        SELECT SUM(requests) req, title
        FROM [fh-bigquery:wikipedia.pagecounts_201408_en_top65k] 
        WHERE REGEXP_MATCH(title, 'Red.*t')
        GROUP BY title
        ORDER BY req DESC
        LIMIT 100 
4. You can't create tables with the free monthly terabyte - it's only for analysis. Activate your free $300 for new Google Cloud Platform accounts, or ask me here to do an extract for you. I will be happy to do so.

Loading data into BigQuery
----

To load data into BigQuery, you will need to activate billing for your project - try it with your free $300 for new accounts.

0. Create a dataset in your project to load the data to: https://i.imgur.com/FRClJ3K.jpg.
1. Find the raw logs shared by Wikipedia at https://dumps.wikimedia.org/other/pagecounts-raw/
2. wget one of these files into your computer, like https://dumps.wikimedia.org/other/pagecounts-raw/2015/2015-06/pagecounts-20150616-160000.gz
3. Install the 'bq' tool. https://cloud.google.com/bigquery/bq-command-line-tool
4. Load it into BigQuery:

        bq load -F" " --quote "" YourProjectName:DatasetName.pagecounts_20150616_16 pagecounts-20150616-160000.gz language,title,requests:integer,content_size:integer

5. Wait a couple minutes. While you wait, let me explain that line: This is not a CSV file, it's a space separated file (-F" ") that doesn't use quotes (--quote ""), we choose a destination table in a dataset in your project (remember to create the dataset first), we chose the file to load, and we define the 4 columns this file has.
6. Note that BigQuery will happily ingest .gz files, up to a certain size. For very large files it's better to un-compress them and put them in Google Cloud Storage first. That's what I did with the [reddit comments](http://reddit.com/r/bigquery/comments/3cej2b/17_billion_reddit_comments_loaded_on_bigquery/) that /u/Stuck_In_The_Matrix compiled. Those files were large, but BigQuery ingested them in 2 minutes or so.

Learn more
----

Ready for more advanced examples? See [how to query Reddit](https://www.reddit.com/r/bigquery/comments/3cej2b/17_billion_reddit_comments_loaded_on_bigquery/) and [how to query the all the NYC taxi trips](https://www.reddit.com/r/bigquery/comments/28ialf/173_million_2013_nyc_taxi_rides_shared_on_bigquery/).

Follow for even more!
----

* Subscribe to [/r/bigquery](http://reddit.com/r/bigquery/top/?sort=top&t=all) (and see the sidebar for more links). 
* Follow me at https://twitter.com/felipehoffa

## Installation or Setup
Detailed instructions on getting google-bigquery set up or installed.

