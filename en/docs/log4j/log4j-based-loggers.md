---
title: "log4j based loggers"
slug: "log4j-based-loggers"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Pros and Cons of different loggers which can be used to create a log4j-formatted log to be viewed using the Log4View viewer.

I will review 3 loggers in this article, Log4cxx, Log4cplus and Log4cpp. 


## Log4cxx
https://logging.apache.org/log4cxx/

 - currently undergoing Incubation - there is no official release
 - update/bug fixes once in the past 12 years, last release was 2008
 - user can select different LogLevels – TRACE, DEBUG, INFO, WARN,
   ERROR, and FATAL
 - hierarchical Loggers
 - it is possible to log asynchronously
 - supports multiple appenders
 - user can select to enabled or disabled the logger
 - log can be sent to different and multiple output targets
 - user selected output formats
 - well documented
 - is licensed under the Apache License, an open source license
   certified by the Open Source Initiative



## Log4cplus
https://sourceforge.net/projects/log4cplus/

 - updates/bug fixes - last release was Jan. 2016
 - user can select select different LogLevels – TRACE, DEBUG, INFO,
   WARN, ERROR, and FATAL hierarchical Loggers
 - supports multi–threaded applications but is not safe to be used from
   asynchronous signals’ handlers
 - user selected output format: SimpleLayout, PatternLayout, TTCCLayout
 - supports multiple loggers
 - not well documented

 

 - is licensed under the Apache License V2.0



## Log4cpp
https://sourceforge.net/projects/log4cpp/

 - bug fixes are about once a year, last release was April 2015
 - supports multi-threaded applications •no clear documentation exist  
 - is licensed under the GNU Lesser General Public License (LGPL) as of
   version 0.2.1, before that have been released under the GPL.

