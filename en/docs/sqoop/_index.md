---
title : sqoop Tutorial
slug : sqoop-tutorial
weight : 9982
draft : false
images : []
type : docs
---

SQOOP Server configuration files are stored in server/config directory of distributed artifact along side with other configuration files of Tomcat (to host SQOOP server).

File `sqoop_bootstrap.properties` specifies which configuration provider should be used for loading configuration for rest of Sqoop server. Default value PropertiesConfigurationProvider should be sufficient.

Second configuration file `sqoop.properties` contains remaining configuration properties that can affect Sqoop server. File is very well documented, so check if all configuration properties fits your environment. Default or very little tweaking should be sufficient most common cases.

