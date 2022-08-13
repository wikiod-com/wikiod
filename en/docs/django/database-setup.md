---
title: "Database Setup"
slug: "database-setup"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Fixtures
Fixtures are initial data for the database. 
The most straightforward way when you have some existing data already is to use the command `dumpdata`

     ./manage.py dumpdata > databasedump.json                # full database
     ./manage.py dumpdata myapp > databasedump.json          # only 1 app
     ./manage.py dumpdata myapp.mymodel > databasedump.json  # only 1 model (table)

This will create a json file which can be imported again by using 

    ./manage.py loaddata databasedump.json

When using the `loadddata` without specifying a file, Django will look for a `fixtures` folder in your app or the list of directories provided in the `FIXTURE_DIRS` in settings, and use its content instead. 

    /myapp
       /fixtures 
            myfixtures.json
            morefixtures.xml 

Possible file formats are: `JSON, XML or YAML`

Fixtures JSON example: 

    [
      {
        "model": "myapp.person",
        "pk": 1,
        "fields": {
          "first_name": "John",
          "last_name": "Lennon"
        }
      },
      {
        "model": "myapp.person",
        "pk": 2,
        "fields": {
          "first_name": "Paul",
          "last_name": "McCartney"
        }
      }
    ]

Fixtures YAML example:

    - model: myapp.person
      pk: 1
      fields:
        first_name: John
        last_name: Lennon
    - model: myapp.person
      pk: 2
      fields:
        first_name: Paul
        last_name: McCartney

Fixtures XML example:

    <?xml version="1.0" encoding="utf-8"?>
    <django-objects version="1.0">
        <object pk="1" model="myapp.person">
            <field type="CharField" name="first_name">John</field>
            <field type="CharField" name="last_name">Lennon</field>
        </object>
        <object pk="2" model="myapp.person">
            <field type="CharField" name="first_name">Paul</field>
            <field type="CharField" name="last_name">McCartney</field>
        </object>
    </django-objects>



## PostgreSQL
Make sure to have some packages installed:

    sudo apt-get install libpq-dev
    pip install psycopg2

Database settings for PostgreSQL:

    #myapp/settings/settings.py
    
    DATABASES = {
        'default': {
            'ENGINE': 'django.db.backends.postgresql',
            'NAME': 'myprojectDB',
            'USER': 'myprojectuser',
            'PASSWORD': 'password',
            'HOST': '127.0.0.1',
            'PORT': '5432',
        }
    }

In older versions you can also use the alias `django.db.backends.postgresql_psycopg2`.


**When using Postresql you'll have access to some extra features:**

*Modelfields:*

    ArrayField         # A field for storing lists of data. 
    HStoreField        # A field for storing mappings of strings to strings. 
    JSONField          # A field for storing JSON encoded data. 
    IntegerRangeField  # Stores a range of integers 
    BigIntegerRangeField # Stores a big range of integers 
    FloatRangeField    # Stores a range of floating point values. 
    DateTimeRangeField # Stores a range of timestamps


## MySQL / MariaDB
*Django supports MySQL 5.5 and higher.*

Make sure to have some packages installed: 

<!-- language: none -->
    $ sudo apt-get install mysql-server libmysqlclient-dev
    $ sudo apt-get install python-dev python-pip              # for python 2
    $ sudo apt-get install python3-dev python3-pip            # for python 3

As well as one of the Python MySQL drivers (`mysqlclient` beeing the recommended choice for Django):

<!-- language: none -->
    $ pip install mysqlclient    # python 2 and 3
    $ pip install MySQL-python   # python 2
    $ pip install pymysql        # python 2 and 3

> The database encoding can not be set by Django, but needs to be
> configured on the database level. Look for `default-character-set` in
> my.cnf (or `/etc/mysql/mariadb.conf/*.cnf` ) and set the encoding:

       [mysql]
       #default-character-set = latin1    #default on some systems.
       #default-character-set = utf8mb4   #default on some systems.
       default-character-set = utf8

       ...
       [mysqld]
       #character-set-server  = utf8mb4
       #collation-server      = utf8mb4_general_ci
       character-set-server  = utf8
       collation-server      = utf8_general_ci
        


  

Database configuration for MySQL or MariaDB

    #myapp/settings/settings.py

    DATABASES = {
        'default': {
            'ENGINE': 'django.db.backends.mysql',
            'NAME': 'DB_NAME',
            'USER': 'DB_USER',
            'PASSWORD': 'DB_PASSWORD',
            'HOST': 'localhost',   # Or an IP Address that your database is hosted on
            'PORT': '3306',
            #optional:
            'OPTIONS': {
                'charset' : 'utf8',
                'use_unicode' : True,
                 'init_command': 'SET '
                    'storage_engine=INNODB,'
                    'character_set_connection=utf8,'
                    'collation_connection=utf8_bin'
                    #'sql_mode=STRICT_TRANS_TABLES,'    # see note below
                    #'SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED',
            },
            'TEST_CHARSET': 'utf8',
            'TEST_COLLATION': 'utf8_general_ci',
        }
    }

If you are using Oracle's MySQL connector your `ENGINE` line should look like this:

    'ENGINE': 'mysql.connector.django',

When you create a database, make sure that to specify the encoding and collation:

<!-- language: sql -->
    CREATE DATABASE mydatabase CHARACTER SET utf8 COLLATE utf8_bin


>  From MySQL 5.7 onwards and on fresh installs of MySQL 5.6, the default value of the sql_mode option contains **STRICT_TRANS_TABLES**. That option escalates warnings into errors when
> data is truncated upon insertion. Django highly recommends activating
> a *strict mode* for MySQL to prevent data loss (either
> STRICT_TRANS_TABLES or STRICT_ALL_TABLES). To enable add to */etc/my.cnf*  `sql-mode = STRICT_TRANS_TABLES`





## Django Cassandra Engine
 - Install pip : 
    `$ pip install django-cassandra-engine`
 - Add Getting Started to INSTALLED_APPS in your settings.py file: 
    `INSTALLED_APPS = ['django_cassandra_engine'] `
 - Cange DATABASES setting Standart:

> Standart

    DATABASES = {
        'default': {
            'ENGINE': 'django_cassandra_engine',
            'NAME': 'db',
            'TEST_NAME': 'test_db',
            'HOST': 'db1.example.com,db2.example.com',
            'OPTIONS': {
                'replication': {
                    'strategy_class': 'SimpleStrategy',
                    'replication_factor': 1
                }
            }
        }
    }

> Cassandra create new user cqlsh :

    DATABASES = {
    'default': {
        'ENGINE': 'django_cassandra_engine',
        'NAME': 'db',
        'TEST_NAME': 'test_db',
        'USER_NAME'='cassandradb',
        'PASSWORD'= '123cassandra',
        'HOST': 'db1.example.com,db2.example.com',
        'OPTIONS': {
            'replication': {
                'strategy_class': 'SimpleStrategy',
                'replication_factor': 1
            }
        }
    }
}

## sqlite
sqlite is the default for Django. 
*It should not be used in production since it is usually slow.* 

    #myapp/settings/settings.py

    DATABASES = {
        'default': {
            'ENGINE': 'django.db.backends.sqlite3',
            'NAME': 'db/development.sqlite3',
            'USER': '',
            'PASSWORD': '',
            'HOST': '',
            'PORT': '',
        },
    }



