---
title: "Read MySQL to DataFrame"
slug: "read-mysql-to-dataframe"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Using sqlalchemy and PyMySQL
    from sqlalchemy import create_engine
    
    cnx = create_engine('mysql+pymysql://username:password@server:3306/database').connect()
    sql = 'select * from mytable'
    df = pd.read_sql(sql, cnx)

## To read mysql to dataframe, In case of large amount of data
To fetch large data we can use generators in pandas and load data in chunks.


    import pandas as pd
    from sqlalchemy import create_engine
    from sqlalchemy.engine.url import URL

    
    # sqlalchemy engine
    engine = create_engine(URL(
        drivername="mysql"
        username="user",
        password="password"
        host="host"
        database="database"
    ))

    conn = engine.connect()
    
    generator_df = pd.read_sql(sql=query,  # mysql query
                               con=conn,
                               chunksize=chunksize)  # size you want to fetch each time

    for dataframe in generator_df:
        for row in dataframe:
            pass  # whatever you want to do

