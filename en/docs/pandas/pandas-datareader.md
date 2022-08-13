---
title: "Pandas Datareader"
slug: "pandas-datareader"
draft: false
images: []
weight: 9695
type: docs
toc: true
---

The Pandas datareader is a sub package that allows one to create a dataframe from various internet datasources, currently including:

 - Yahoo! Finance
 - Google Finance
 - St.Louis FED (FRED)
 - Kenneth French’s data library
 - World Bank
 - Google Analytics

For more information, [see here][1].


  [1]: http://pandas.pydata.org/pandas-docs/stable/remote_data.html#remote-data-access

## Datareader basic example (Yahoo Finance)
    from pandas_datareader import data
    
    # Only get the adjusted close.
    aapl = data.DataReader("AAPL", 
                           start='2015-1-1', 
                           end='2015-12-31', 
                           data_source='yahoo')['Adj Close']

    >>> aapl.plot(title='AAPL Adj. Closing Price')
[![enter image description here][1]][1]

    # Convert the adjusted closing prices to cumulative returns.
    returns = aapl.pct_change()
    >>> ((1 + returns).cumprod() - 1).plot(title='AAPL Cumulative Returns')

[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/amLQD.png
  [2]: http://i.stack.imgur.com/JiPUS.png


## Reading financial data (for multiple tickers) into pandas panel - demo
    from datetime import datetime
    import pandas_datareader.data as wb
    
    stocklist = ['AAPL','GOOG','FB','AMZN','COP']
    
    start = datetime(2016,6,8)
    end = datetime(2016,6,11)
    
    p = wb.DataReader(stocklist, 'yahoo',start,end)

`p` - is a pandas panel, with which we can do funny things:

let's see what do we have in our panel
    
    In [388]: p.axes
    Out[388]:
    [Index(['Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close'], dtype='object'),
     DatetimeIndex(['2016-06-08', '2016-06-09', '2016-06-10'], dtype='datetime64[ns]', name='Date', freq='D'),
     Index(['AAPL', 'AMZN', 'COP', 'FB', 'GOOG'], dtype='object')]
    
    In [389]: p.keys()
    Out[389]: Index(['Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close'], dtype='object')

selecting & slicing data

    In [390]: p['Adj Close']
    Out[390]:
                     AAPL        AMZN        COP          FB        GOOG
    Date
    2016-06-08  98.940002  726.640015  47.490002  118.389999  728.280029
    2016-06-09  99.650002  727.650024  46.570000  118.559998  728.580017
    2016-06-10  98.830002  717.909973  44.509998  116.620003  719.409973
    
    In [391]: p['Volume']
    Out[391]:
                      AAPL       AMZN        COP          FB       GOOG
    Date
    2016-06-08  20812700.0  2200100.0  9596700.0  14368700.0  1582100.0
    2016-06-09  26419600.0  2163100.0  5389300.0  13823400.0   985900.0
    2016-06-10  31462100.0  3409500.0  8941200.0  18412700.0  1206000.0
    
    In [394]: p[:,:,'AAPL']
    Out[394]:
                     Open       High        Low      Close      Volume  Adj Close
    Date
    2016-06-08  99.019997  99.559998  98.680000  98.940002  20812700.0  98.940002
    2016-06-09  98.500000  99.989998  98.459999  99.650002  26419600.0  99.650002
    2016-06-10  98.529999  99.349998  98.480003  98.830002  31462100.0  98.830002
    
    In [395]: p[:,'2016-06-10']
    Out[395]:
                Open        High         Low       Close      Volume   Adj Close
    AAPL   98.529999   99.349998   98.480003   98.830002  31462100.0   98.830002
    AMZN  722.349976  724.979980  714.210022  717.909973   3409500.0  717.909973
    COP    45.900002   46.119999   44.259998   44.509998   8941200.0   44.509998
    FB    117.540001  118.110001  116.260002  116.620003  18412700.0  116.620003
    GOOG  719.469971  725.890015  716.429993  719.409973   1206000.0  719.409973


