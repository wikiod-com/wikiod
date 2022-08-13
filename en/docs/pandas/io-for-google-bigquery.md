---
title: "IO for Google BigQuery"
slug: "io-for-google-bigquery"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Reading data from BigQuery with user account credentials
    In [1]: import pandas as pd

In order to run a query in BigQuery you need to have your own BigQuery project. We can request some public sample data:

    In [2]: data = pd.read_gbq('''SELECT title, id, num_characters
       ...:                       FROM [publicdata:samples.wikipedia]
       ...:                       LIMIT 5'''
       ...:                    , project_id='<your-project-id>')

This will print out:

    Your browser has been opened to visit:
    
        https://accounts.google.com/o/oauth2/v2/auth...[looong url cutted]
    
    If your browser is on a different machine then exit and re-run this
    application with the command-line parameter
    
      --noauth_local_webserver

If your are operating from local machine than browser will pop-up. After granting privileges pandas will continue with output:

    Authentication successful.
    Requesting query... ok.
    Query running...
    Query done.
    Processed: 13.8 Gb
    
    Retrieving results...
    Got 5 rows.
    
    Total time taken 1.5 s.
    Finished at 2016-08-23 11:26:03.

Result:

    In [3]: data
    Out[3]: 
                   title       id  num_characters
    0       Fusidic acid   935328            1112
    1     Clark Air Base   426241            8257
    2  Watergate scandal    52382           25790
    3               2005    35984           75813
    4               .BLP  2664340            1659

As a side effect pandas will create json file `bigquery_credentials.dat` which will allow you to run further queries without need to grant privileges any more:

    In [9]: pd.read_gbq('SELECT count(1) cnt FROM [publicdata:samples.wikipedia]'
                       , project_id='<your-project-id>')
    Requesting query... ok.
    [rest of output cutted]

    Out[9]: 
             cnt
    0  313797035
    

## Reading data from BigQuery with service account credentials
If you have created [service account][1] and have private key json file for it, you can use this file to authenticate with pandas

    In [5]: pd.read_gbq('''SELECT corpus, sum(word_count) words
                           FROM [bigquery-public-data:samples.shakespeare]       
                           GROUP BY corpus                                
                           ORDER BY words desc
                           LIMIT 5'''
                       , project_id='<your-project-id>'
                       , private_key='<private key json contents or file path>')
    Requesting query... ok.
    [rest of output cutted]

    Out[5]: 
               corpus  words
    0          hamlet  32446
    1  kingrichardiii  31868
    2      coriolanus  29535
    3       cymbeline  29231
    4    2kinghenryiv  28241
    


  [1]: https://console.developers.google.com/permissions/serviceaccounts

