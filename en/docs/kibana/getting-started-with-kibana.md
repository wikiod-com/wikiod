---
title: "Getting started with kibana"
slug: "getting-started-with-kibana"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
**What is Kibana:**
           


----------


Kibana is used for making visualizations and creating dashboards for the indexes presented in elasticsearch. Basically, it is an open source plug-in for elasticsearch.

[![Kibana Page][1]][1]

There are Six Tabs:

 1. Discover:

     You can explore your data from Discover tab
 2. Visulization:

     Creating visualization with different charts such as bar chart,line chart,pie chart etc.
 3. Dashboard:

    It is used for creating dashboard with created visualization. It contains your visualization to represent your process.
 4. Timelion:

    It is timeseries of kibana
 5. Dev Tools

    It is like sense plugin used for elasticsearch. It contains request and response field 
 6. Management

    This is used for managing your kibana. It contains Settings for the kibana plugin


  [1]: https://i.stack.imgur.com/vMgva.png

## Installation
> prerequisites:

 - To run kibana you need to install supported version of elastic search.

for install elastic search refer this link [Elasticsearch documentation][1]

| Kibana | Elasticsearch |
| ------ | ------ |
| 4.0.0 - 4.1.x  | 1.4.x - 1.7.x  |
| 4.2.x | 2.0.x  |
|4.3.x  | 2.1.x  |
| 4.4.x  | 2.2.x  |
| 4.5.x  | 2.3.x  |
|4.6.x | 2.4.x  |
| 5.x  | 5.x  |



 - The Kibana can be downloaded from this link: 
[Kibana Downloads][2]

choose the file that supports your operating system

 - For past realese of kibana use this webpage
[kibana past release][3]

**Install kibana using apt-get**

To install kibana you need to find out which version of elastic search is installed on your system. That can be checked using the following command:

    curl "http://localhost:9200"

In the output you need to check the number i.e. the elastic search version. 

Now find the suitable version for that. Here is the command for installing kibana 5.4.0 for elastic search 5.4.0. 

    wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -
    sudo apt-get install apt-transport-https
    echo "deb https://artifacts.elastic.co/packages/5.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-5.x.list
    sudo apt-get update && sudo apt-get install kibana

**Run Kibana as a service**

After Installation Kiabana doesn't start running automatically. How to start & stop depends on whether your system supports SysV init or systemd. check that using the following command. 

    ps -p 1

**For Sysv init** 

execute the following command:

    sudo update-rc.d kibana defaults 95 10

To start & stop the server use the following command.

    sudo -i service kibana start
    sudo -i service kibana stop

**For systemd :**

execute the following commands:

    sudo /bin/systemctl daemon-reload
    sudo /bin/systemctl enable kibana.service
To start & stop the server use the following command.

    sudo systemctl start kibana.service
    sudo systemctl stop kibana.service

  [1]: https://www.elastic.co/support/matrix#show_compatibility
  [2]: https://www.elastic.co/downloads/kibana
  [3]: https://www.elastic.co/downloads/past-releases




## Setup
For kibana configuration open config/kibana.yml and point your elasticsearch address.

    By default it is http://localhost:9200
Before starting elasticsearch check whether elasticsearch is running. Becuacuse it is relies on Elasticsearch.

> For windows:

run bin/kibana patch file.

> For Mac:

run ./kibana

> For Linux:

run bin/kibana.

 - To Start kibana

   
     Kibana URL address: http://localhost:5601/

