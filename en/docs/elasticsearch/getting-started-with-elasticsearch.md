---
title: "Getting started with Elasticsearch"
slug: "getting-started-with-elasticsearch"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing Elasticsearch on Windows
# Prerequisites

The Windows version of Elasticsearch can be obtained from this link: https://www.elastic.co/downloads/elasticsearch. The latest stable release is always at the top. 

As we are installing on Windows, we need the `.ZIP` archive. Click the link in the `Downloads:` section and save the file to your computer.

This version of elastic is "portable", meaning you don't need to run an installer to use the program. Unzip the contents of the file to a location you can easily remember. For demonstration we'll assume you unzipped everything to `C:\elasticsearch`.

Note that the archive contains a folder named `elasticsearch-<version>` by default, you can either extract that folder to `C:\` and rename it to `elasticsearch` or create `C:\elasticsearch` yourself, then unzip only the *contents* of the folder in the archive to there.

Because Elasticsearch is written in Java, it needs the Java Runtime Environment to function. So before running the server, check if Java is available by opening a command prompt and typing:

    java -version

You should get a response that looks like this:

    java version "1.8.0_91"
    Java(TM) SE Runtime Environment (build 1.8.0_91-b14)
    Java HotSpot(TM) Client VM (build 25.91-b14, mixed mode)

If you see the following instead

 > 'java' is not recognized as an internal or external command, operable program or batch file.

Java is not installed on your system or is not configured properly. You can follow [this tutorial](https://java.com/en/download/help/windows_manual_download.xml) to (re)install Java. Also, make sure that these environment variables are set to similar values:

| Variable  | Value                              |
| --------- | ---------------------------------- |
| JAVA_HOME | C:\Program Files\Java\jre          |
| PATH      | &hellip;;C:\Program Files\Java\jre |

If you don't yet know how to inspect these variables consult [this tutorial](https://www.java.com/en/download/help/path.xml).

# Run from batch file

With Java installed, open the `bin` folder. It can be found directly within the folder you  unzipped everything to, so it should be under `c:\elasticsearch\bin`. Within this folder is a file called `elasticsearch.bat` which can be used to start Elasticsearch in a command window. This means that information logged by the process will be visible in the command prompt window. To stop the server, press <kbd>CTRL</kbd><kbd>C</kbd> or simply close the window.

# Run as a Windows service

Ideally you don't want to have an extra window you can't get rid of during development, and for this reason, Elasticsearch can be configured to run as a service.

Before we could install Elasticsearch as a service we need to add a line to the file `C:\elasticsearch\config\jvm.options`:

>The service installer requires that the thread stack size setting be configured in `jvm.options` before you install the service. On 32-bit Windows, you should add `-Xss320k` [&hellip;] and on 64-bit Windows you should add `-Xss1m` to the `jvm.options` file. <sup>[[source]](https://www.elastic.co/guide/en/elasticsearch/reference/current/windows.html#windows-service)</sup>

Once you made that change, open a command prompt and navigate to the `bin` directory by running the following command:

    C:\Users\user> cd c:\elasticsearch\bin


Service management is handled by `elasticsearch-service.bat`. In older versions this file might simply be called `service.bat`. To see all available arguments, run it without any:

    C:\elasticsearch\bin> elasticsearch-service.bat

    Usage: elasticsearch-service.bat install|remove|start|stop|manager [SERVICE_ID]

The output also tells us that there's an optional `SERVICE_ID` argument, but we can ignore it for now. To install the service, simply run:

    C:\elasticsearch\bin> elasticsearch-service.bat install

After installing the service, you can start and stop it with the respective arguments. To start the service, run

    C:\elasticsearch\bin> elasticsearch-service.bat start

and to stop it, run

    C:\elasticsearch\bin> elasticsearch-service.bat stop

If you prefer a GUI to manage the service instead, you can use the following command:

    C:\elasticsearch\bin> elasticsearch-service.bat manager

This will open the Elastic Service Manager, which allows you to customize some service-related settings as well as stop/start the service using the buttons found at the bottom of the first tab.

## Installing Elasticsearch on Ubuntu 14.04
<br>

Prerequisites
----------------------------
In order to run Elasticsearch, a Java Runtime Environment (JRE) is required on the machine. Elasticsearch requires Java 7 or higher and recommends `Oracle JDK version 1.8.0_73`.

**Install Oracle Java 8**

    sudo add-apt-repository -y ppa:webupd8team/java
    sudo apt-get update
    echo "oracle-java8-installer shared/accepted-oracle-license-v1-1 select true" | sudo debconf-set-selections
    sudo apt-get install -y oracle-java8-installer

**Check Java Version**

    java -version

Download and Install package
----------------------------

**Using Binaries**

1. Download the latest stable version of Elasticsearch [here](https://www.elastic.co/downloads/elasticsearch).
2. Unzip the file & Run

Linux:

    $ bin/elasticsearch


**Using apt-get**

An alternative to downloading elasticsearch from the website is installing it, using `apt-get`.

    wget -qO - https://packages.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -
    echo "deb https://packages.elastic.co/elasticsearch/2.x/debian stable main" | sudo tee -a /etc/apt/sources.list.d/elasticsearch-2.x.list
    sudo apt-get update && sudo apt-get install elasticsearch
    sudo /etc/init.d/elasticsearch start

<br>

Installing elasticsearch version 5.x

    
    wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -
    sudo apt-get install apt-transport-https
    echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-5.x.list
    sudo apt-get update && sudo apt-get install elasticsearch

Running as a service on Linux:
----------------------------
After Installing the above doesn't start itself. so we need to start it as a service. How to start or stop Elasticsearch depends on whether your system uses SysV init or systemd. you can check it with the following command.

    ps -p 1

 If your distribution is using SysV init, then you will need to run:

    sudo update-rc.d elasticsearch defaults 95 10
    sudo /etc/init.d/elasticsearch start

Otherwise if your distribution is using systemd:

    sudo /bin/systemctl daemon-reload
    sudo /bin/systemctl enable elasticsearch.service

Run the `CURL` command from your browser or a REST client, to check if Elasticsearch has been installed correctly.

    curl -X GET http://localhost:9200/

## Indexing and retrieving a document
Elasticsearch is accessed through a HTTP REST API, typically using the cURL library. The messages between the search server and the client (your or your application) are sent in the form of JSON strings. By default, Elasticsearch runs on port 9200.

In the examples below, `?pretty` is added to tell Elasticsearch to prettify the JSON response. When using these endpoints within an application you needn't add this query parameter.

# Indexing documents

If we intend to update information within an index later, it's a good idea to assign unique IDs to the documents we index. To add a document to the index named `megacorp`, with type `employee` and ID `1` run:

  ```
  curl -XPUT "http://localhost:9200/megacorp/employee/1?pretty" -d'
  {
      "first_name" : "John",
      "last_name" :  "Smith",
      "age" :        25,
      "about" :      "I love to go rock climbing",
      "interests": [ "sports", "music" ]
  }' 
  ```

Response:

<!-- language: lang-json -->

  ```
  {
    "_index": "megacorp",
    "_type": "employee",
    "_id": "1",
    "_version": 1,
    "_shards": {
      "total": 2,
      "successful": 1,
      "failed": 0
    },
    "created": true
  }

  ```
  The index is created if it does not exist when we send the PUT call.

## Indexing without an ID
  ```
  POST /megacorp/employee?pretty
  {
      "first_name" :  "Jane",
      "last_name" :   "Smith",
      "age" :         32,
      "about" :       "I like to collect rock albums",
      "interests":  [ "music" ]
  }
  ```
  Response:
  <!-- language: lang-json -->
  ```
  {
    "_index": "megacorp",
    "_type": "employee",
    "_id": "AVYg2mBJYy9ijdngfeGa",
    "_version": 1,
    "_shards": {
      "total": 2,
      "successful": 2,
      "failed": 0
    },
    "created": true
  }
  
  ```
# Retrieving documents

  ```
  curl -XGET "http://localhost:9200/megacorp/employee/1?pretty"
  ```
  Response:
  <!-- language: lang-json -->
  ```
  {
    "_index": "megacorp",
    "_type": "employee",
    "_id": "1",
    "_version": 1,
    "found": true,
    "_source": {
      "first_name": "John",
      "last_name": "Smith",
      "age": 25,
      "about": "I love to go rock climbing",
      "interests": [
        "sports",
        "music"
      ]
    }
  }

  ```

Fetch 10 documents from the `megacorp` index with the type `employee`:

  ```
  curl -XGET "http://localhost:9200/megacorp/employee/_search?pretty"
  ```
  Response:
  <!-- language: lang-json -->
  ```
  {
    "took": 2,
    "timed_out": false,
    "_shards": {
      "total": 5,
      "successful": 5,
      "failed": 0
    },
    "hits": {
      "total": 2,
      "max_score": 1,
      "hits": [
        {
          "_index": "megacorp",
          "_type": "employee",
          "_id": "1",
          "_score": 1,
          "_source": {
            "first_name": "John",
            "last_name": "Smith",
            "age": 25,
            "about": "I love to go rock climbing",
            "interests": [
              "sports",
              "music"
            ]
          }
        },
        {
          "_index": "megacorp",
          "_type": "employee",
          "_id": "AVYg2mBJYy9ijdngfeGa",
          "_score": 1,
          "_source": {
            "first_name": "Jane",
            "last_name": "Smith",
            "age": 32,
            "about": "I like to collect rock albums",
            "interests": [
              "music"
            ]
          }
        }
      ]
    }
  }

  ```

Simple search using the `match` query, which looks for exact matches in the field provided:


  ```
  curl -XGET "http://localhost:9200/megacorp/employee/_search" -d'
  {
      "query" : {
          "match" : {
              "last_name" : "Smith"
          }
      }
  }'
  ```
  Response:
  <!-- language: lang-json -->
  ```
  {
    "took": 2,
    "timed_out": false,
    "_shards": {
      "total": 5,
      "successful": 5,
      "failed": 0
    },
    "hits": {
      "total": 1,
      "max_score": 0.6931472,
      "hits": [
        {
          "_index": "megacorp",
          "_type": "employee",
          "_id": "1",
          "_score": 0.6931472,
          "_source": {
            "first_name": "John",
            "last_name": "Smith",
            "age": 25,
            "about": "I love to go rock climbing",
            "interests": [
              "sports",
              "music"
            ]
          }
        }
      ]
    }
  }

  ```


## Installing Elasticsearch and Kibana on CentOS 7
In order to run Elasticsearch, a Java Runtime Environment (JRE) is required on the machine. Elasticsearch requires Java 7 or higher and recommends `Oracle JDK version 1.8.0_73`.

So, be sure if you have Java in your system. If not, then follow the procedure:

    # Install wget with yum 
    yum -y install wget

    # Download the rpm jre-8u60-linux-x64.rpm for 64 bit 
    wget --no-cookies --no-check-certificate --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u60-b27/jre-8u60-linux-x64.rpm"

    # Download the rpm jre-8u101-linux-i586.rpm for 32 bit
    wget --no-cookies --no-check-certificate --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u101-b13/jre-8u101-linux-i586.rpm"
 
    # Install jre-.*.rpm
    rpm -ivh jre-.*.rpm


Java should be installed by now in your centOS system. You can check it with:

    java -version

**Download & install elasticsearch**

    # Download elasticsearch-2.3.5.rpm 
    wget https://download.elastic.co/elasticsearch/release/org/elasticsearch/distribution/rpm/elasticsearch/2.3.5/elasticsearch-2.3.5.rpm

    # Install elasticsearch-.*.rpm
    rpm -ivh elasticsearch-.*.rpm   

**Running elasticsearch as a systemd service on startup**

    
    sudo systemctl daemon-reload
    sudo systemctl enable elasticsearch
    sudo systemctl start elasticsearch

    # check the current status to ensure everything is okay.
    systemctl status elasticsearch

**Installing Kibana**

First  import GPG-key on rpm

    sudo rpm --import http://packages.elastic.co/GPG-KEY-elasticsearch
    
Then create a local repository `kibana.repo` 

    sudo vi /etc/yum.repos.d/kibana.repo 


And Add the following content:
 
    [kibana-4.4]
    name=Kibana repository for 4.4.x packages
    baseurl=http://packages.elastic.co/kibana/4.4/centos
    gpgcheck=1
    gpgkey=http://packages.elastic.co/GPG-KEY-elasticsearch
    enabled=1
        
Now install the kibana by following command:

    yum -y install kibana


Start it with:

    systemctl start kibana

Check status with:

    systemctl status kibana


You may run it as a startup service.

    systemctl enable kibana





   


## Basic Search Parameters with examples:
By default, the full indexed document is returned as part of all searches. This is referred to as the source (`_source` field in the search hits). If we don’t want the entire source document returned, we have the ability to request only a few fields from within source to be returned, or we can set `_source` to false to omit the field entirely.

This example shows how to return two fields, `account_number` and `balance` (inside of `_source`), from the search:

    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": { "match_all": {} },
      "_source": ["account_number", "balance"]
    }'

Note that the above example simply reduces the information returned in the `_source` field. It will still only return one field named `_source` but only the fields `account_number` and `balance` will be included.

If you come from a SQL background, the above is somewhat similar in concept to the SQL query

    SELECT account_number, balance FROM bank;

Now let’s move on to the query part. Previously, we’ve seen how the `match_all` query is used to match all documents. Let’s now introduce a new query called the match query, which can be thought of as a basic fielded search query (i.e. a search done against a specific field or set of fields).

This example returns the account with the `account_number` set to `20`:

    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": { "match": { "account_number": 20 } }
    }'

This example returns all accounts containing the term "mill" in the `address`:
    
    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": { "match": { "address": "mill" } }
    }'

This example returns all accounts containing the term "mill" or "lane" in the `address`:

    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": { "match": { "address": "mill lane" } }
    }'

This example is a variant of `match` (`match_phrase`) that splits the query into terms and only returns documents that contain all terms in the `address` in the same positions relative to each other<sup>[[1]](https://www.elastic.co/guide/en/elasticsearch/guide/current/phrase-matching.html#phrase-matching)</sup>.


    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": { "match_phrase": { "address": "mill lane" } }
    }'

Let’s now introduce the bool(ean) query. The bool query allows us to compose smaller queries into bigger queries using boolean logic.

This example composes two match queries and returns all accounts containing "mill" and "lane" in the address:


    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": {
        "bool": {
          "must": [
            { "match": { "address": "mill" } },
            { "match": { "address": "lane" } }
          ]
        }
      }
    }'


In the above example, the bool `must` clause specifies all the queries that must be true for a document to be considered a match.

In contrast, this example composes two match queries and returns all accounts containing "mill" or "lane" in the `address`:

    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": {
        "bool": {
          "should": [
            { "match": { "address": "mill" } },
            { "match": { "address": "lane" } }
          ]
        }
      }
    }'

In the above example, the bool `should` clause specifies a list of queries either of which must be true for a document to be considered a match.

This example composes two match queries and returns all accounts that contain neither "mill" nor "lane" in the `address`:

    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": {
        "bool": {
          "must_not": [
            { "match": { "address": "mill" } },
            { "match": { "address": "lane" } }
          ]
        }
      }
    }'

In the above example, the bool must_not clause specifies a list of queries none of which must be true for a document to be considered a match.

We can combine must, should, and must_not clauses simultaneously inside a bool query. Furthermore, we can compose bool queries inside any of these bool clauses to mimic any complex multi-level boolean logic.

This example returns all accounts that belong to people who are exactly 40 years old and don’t live in Washington (`WA` for short):

    curl -XPOST 'localhost:9200/bank/_search?pretty' -d '
    {
      "query": {
        "bool": {
          "must": [
            { "match": { "age": "40" } }
          ],
          "must_not": [
            { "match": { "state": "WA" } }
          ]
        }
      }
    }'

