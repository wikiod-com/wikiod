---
title: "Read SQL Server to Dataframe"
slug: "read-sql-server-to-dataframe"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

## Using pyodbc
    import pandas.io.sql
    import pyodbc
    import pandas as pd
    


Specify the parameters

    # Parameters
    server = 'server_name'
    db = 'database_name'
    UID = 'user_id'

Create the connection

    # Create the connection
    conn = pyodbc.connect('DRIVER={SQL Server};SERVER=' + server + ';DATABASE=' + db + '; UID = ' + UID + '; PWD = ' + UID + 'Trusted_Connection=yes')

Query into pandas dataframe

    # Query into dataframe
    df= pandas.io.sql.read_sql('sql_query_string', conn)

## Using pyodbc with connection loop
    import os, time
    import pyodbc
    import pandas.io.sql as pdsql

    def todf(dsn='yourdsn', uid=None, pwd=None, query=None, params=None):
        ''' if `query` is not an actual query but rather a path to a text file 
            containing a query, read it in instead '''
        if query.endswith('.sql') and os.path.exists(query):
            with open(query,'r') as fin:
                query = fin.read()
            
        connstr = "DSN={};UID={};PWD={}".format(dsn,uid,pwd)
        connected = False
        while not connected:
            try:
                with pyodbc.connect(connstr,autocommit=True) as con:
                    cur = con.cursor()
                    if params is not None: df = pdsql.read_sql(query, con, 
                                                               params=params)
                    else: df = pdsql.read_sql(query, con)
                    cur.close()
                break 
            except pyodbc.OperationalError:
                time.sleep(60) # one minute could be changed
        return df

