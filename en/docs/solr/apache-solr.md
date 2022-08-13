---
title: "Apache Solr"
slug: "apache-solr"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Solr installation
You can install Solr in any system where a suitable Java Runtime Environment (JRE) is available, as detailed below. Currently this includes Linux, OS X, and Microsoft Windows.
You will need the Java Runtime Environment (JRE) version 1.8 or higher. At a command line, check your Java version like this:

$ java -version

Solr is available from the Solr website at http://lucene.apache.org/solr/.  Extract the Solr distribution archive to your local directory

## Solr quick start (start and stop solar)


You can start Solr by running bin/solr from the Solr directory and this will start Solr in the background, listening on port 8983.

$ bin/solr start

To change the port Solr listens on, you can use the -p parameter when starting, such as:

$ bin/solr start -p 8984

Since Solr is a server, it is more common to run it in the background, especially on Unix/Linux. However, to start Solr in the foreground, simply do:

$ bin/solr start -f

When running Solr in the foreground (using -f), then you can stop it using Ctrl-c. However, when running in the background, you should use the stop command, such as:

$ bin/solr stop -p 8983

The stop command requires you to specify the port Solr is listening on or you can use the -all parameter to stop all running Solr instances.

If you're not sure if Solr is running locally, you can use the status command:

$ bin/solr status

