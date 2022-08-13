---
title: "Connecting scrapy to MySQL"
slug: "connecting-scrapy-to-mysql"
draft: false
images: []
weight: 9864
type: docs
toc: true
---

## Connecting and bulk-inserting to MySQL in Scrapy using MySQLDB module - Python 2.7
This example demonstrate how to dynamically insert data into MySQL using Python Scrapy.

You do not need to edit `pipelines.py` file for any project.

This example can be used for all your project. 

Just `yield you_data_dictionary` from your `Spider` and inside `pipelines.py` a query will be created automatically.

Rows are inserted in bulk using bulk insert statement.

**MUST READ**:

 1. Keys of you `you_data_dictionary` that you are `yield`ing from Spider must be same as your column names of database table.
 2. Table must be created before you run your code.
 3. Notice `if len(self.items) >= 50` line, you can change `50` to any integer.

<h2>**`settings.py`**</h2>

    DB_CREDS = {
        'host':'localhost',
        'user':'root',
        'pass':'password',
        'db':'db_name'
    }

<h2>**`your_project_folder/spiders/spider_file.py`**</h2>

    from scrapy.utils.project import get_project_settings
            def __init__(self, *args, **kwargs):
                    self.connectDB()
    
            def connectDB(self):
                    
                    self.conn = MySQLdb.connect(user=DB_CREDS['user'], passwd=DB_CREDS['pass'], db=DB_CREDS['db'], host=DB_CREDS['host'], charset="utf8", use_unicode=True)
    
                    self.cursor = MySQLdb.cursors.DictCursor(self.conn) 
    
                    self.conn.autocommit(True)


<h2>**`your_project_folder/pipelines.py`**</h2>
         

    # -*- coding: utf-8 -*-
    import logging
    from scrapy import signals
    
    class MyPipeline(object):
    
        def __init__(self):
            self.items=[]
    
        def process_item(self, item, spider):
                
                    self.placeholders = ', '.join(['%s'] * len(item))
                    self.columns = ', '.join(item.keys())
                    self.query = "INSERT INTO %s ( %s ) VALUES ( %s )" % ("table_name", self.columns, self.placeholders)
                    
            self.items.extend([item.values()])
            
            if len(self.items) >= 50:
    
                try:
                    spider.cursor.executemany(self.query, self.items)
                    self.items = []    
                except Exception as e:
                    if 'MySQL server has gone away' in str(e):
                        spider.connectDB()
                        spider.cursor.executemany(self.query, self.items)
                        self.items = []    
                        else:   
                            raise e
            return item
    
    
    
            def close_spider(self, spider):
                try:
                        spider.cursor.executemany(self.query, self.items)
                        self.items = []    
                except Exception as e:
                        if 'MySQL server has gone away' in str(e):
                                spider.connectDB()
                                spider.cursor.executemany(self.query, self.items)
                                self.items = []    
                        else:   
                                raise e

## Connecting scrapy to MySQL (Windows 8 pro 64-bit, python 2.7, scrapy v 1.2)
The following example is tested on **Windows 8 pro 64-bit** operating system with **python 2.7** and **scrapy v 1.2**. Let assume that we have already installed the scrapy framework.

**MySQL database that we will use in the following tutorial**

    CREATE TABLE IF NOT EXISTS `scrapy_items` (
      `id` bigint(20) UNSIGNED NOT NULL,
      `quote` varchar(255) NOT NULL,
      `author` varchar(255) NOT NULL,
      PRIMARY KEY (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=latin1;

    INSERT INTO `scrapy_items` (`id`, `quote`, `author`) 
    VALUES (1, 'The world as we have created it is a process of our thinking. It cannot be changed without changing our thinking.', 'Albert Einstein');

**Installation MySQL driver**
1. Download driver [mysql-connector-python-2.2.1.zip][1] OR [MySQL-python-1.2.5.zip (md5)][2]
2. Extract zip into a file e.g **C:\mysql-connector**\
3. Open **cmd** go to the **C:\mysql-connector** where **setup.py** file will be located and run **python setup.py install**
4. Copy and run the following **example.py**

>     from __future__ import print_function
>     import mysql.connector
>     from mysql.connector import errorcode
>     
>     class MysqlTest():
>         table = 'scrapy_items'
>         conf = {
>             'host': '127.0.0.1',
>             'user': 'root',
>             'password': '',
>             'database': 'test',
>             'raise_on_warnings': True
>         }
>         
>         def __init__(self, **kwargs):
>             self.cnx = self.mysql_connect()
>         
>         def mysql_connect(self):
>             try:
>                 return mysql.connector.connect(**self.conf)
>             except mysql.connector.Error as err:
>                 if err.errno == errorcode.ER_ACCESS_DENIED_ERROR:
>                     print("Something is wrong with your user name or password")
>                 elif err.errno == errorcode.ER_BAD_DB_ERROR:
>                     print("Database does not exist")
>                 else:
>                     print(err)
>     
>         def select_item(self):
>             cursor = self.cnx.cursor()
>             select_query = "SELECT * FROM " + self.table
>     
>             cursor.execute(select_query)
>             for row in cursor.fetchall():
>                 print(row)
>     
>             cursor.close()
>             self.cnx.close()
>     
>     def main():
>         mysql = MysqlTest()
>         mysql.select_item()
>     
>     if __name__ == "__main__" : main()

**Connect Scrapy to MySQL**

First create a new scrapy project by running the following command

    scrapy startproject tutorial

This will create a tutorial directory with the following contents:

[![enter image description here][3]][3]

This is the code for our first Spider. Save it in a file named **quotes_spider.py** under the **tutorial/spiders** directory in your project.

**Our first Spider**

    import scrapy
    from scrapy.loader import ItemLoader
    from tutorial.items import TutorialItem
    
    class QuotesSpider(scrapy.Spider):
        name = "quotes"
    
        def start_requests(self):
            urls = ['http://quotes.toscrape.com/page/1/']
            for url in urls:
                yield scrapy.Request(url=url, callback=self.parse)
    
        def parse(self, response):
            boxes = response.css('div[class="quote"]')
            for box in boxes:
                item = ItemLoader(item=TutorialItem())
                quote = box.css('span[class="text"]::text').extract_first()
                author = box.css('small[class="author"]::text').extract_first()
                item.add_value('quote', quote.encode('ascii', 'ignore'))
                item.add_value('author', author.encode('ascii', 'ignore'))
                yield item.load_item()

**Scrapy Item Class**

To define common output data format Scrapy provides the **Item** class. **Item** objects are simple containers used to collect the scraped data and specify metadata for the field. They provide a **dictionary-like** API with a convenient syntax for declaring their available fields. For detail [click me][4]

    import scrapy
    from scrapy.loader.processors import TakeFirst
    
    class TutorialItem(scrapy.Item):
        # define the fields for your item here like:
        quote = scrapy.Field(output_processor=TakeFirst(),)
        author = scrapy.Field(output_processor=TakeFirst(),)

**Scrapy Pipeline**

After an item has been scraped by a spider, it is sent to the Item Pipeline which processes it through several components that are executed sequentially and this is the place where we save our scraped data into database. For detail [click me][5]

> **Note**: Don't forget to add your pipeline to the **ITEM_PIPELINES** setting located in **tutorial/tutorial/settings.py** file.

    from __future__ import print_function
    import mysql.connector
    from mysql.connector import errorcode
    
    class TutorialPipeline(object):
        table = 'scrapy_items'
        conf = {
            'host': '127.0.0.1',
            'user': 'root',
            'password': '',
            'database': 'sandbox',
            'raise_on_warnings': True
        }
        
        def __init__(self, **kwargs):
            self.cnx = self.mysql_connect()
    
        def open_spider(self, spider):
            print("spider open")
    
        def process_item(self, item, spider):
            print("Saving item into db ...")
            self.save(dict(item))
            return item
        
        def close_spider(self, spider):
            self.mysql_close()
        
        def mysql_connect(self):
            try:
                return mysql.connector.connect(**self.conf)
            except mysql.connector.Error as err:
                if err.errno == errorcode.ER_ACCESS_DENIED_ERROR:
                    print("Something is wrong with your user name or password")
                elif err.errno == errorcode.ER_BAD_DB_ERROR:
                    print("Database does not exist")
                else:
                    print(err)
        
        
        def save(self, row): 
            cursor = self.cnx.cursor()
            create_query = ("INSERT INTO " + self.table + 
                "(quote, author) "
                "VALUES (%(quote)s, %(author)s)")
    
            # Insert new row
            cursor.execute(create_query, row)
            lastRecordId = cursor.lastrowid
    
            # Make sure data is committed to the database
            self.cnx.commit()
            cursor.close()
            print("Item saved with ID: {}" . format(lastRecordId)) 
    
        def mysql_close(self):
            self.cnx.close()

Ref:
https://doc.scrapy.org/en/latest/index.html

  [1]: http://dev.mysql.com/downloads/file/?id=465641
  [2]: https://pypi.python.org/pypi/MySQL-python/1.2.5
  [3]: https://i.stack.imgur.com/e7SqL.jpg
  [4]: https://doc.scrapy.org/en/1.2/topics/items.html
  [5]: https://doc.scrapy.org/en/latest/topics/item-pipeline.html


