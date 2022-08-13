---
title : Bosun Tutorial
slug : bosun-tutorial
weight : 9910
draft : false
images : []
type : docs
---

Bosun is an open-source, MIT licensed, monitoring and alerting system created by Stack Overflow. It has an expressive domain specific language for evaluating alerts and creating detailed notifications. It also lets you test your alerts against historical data for a faster development experience. More details at http://bosun.org/.

Bosun uses a config file to store all the system settings, macros, lookups, notifications, templates, and alert definitions. You specify the config file to use when starting the server, for example `/opt/bosun/bosun -c /opt/bosun/config/prod.conf`. Changes to the file will not be activated until bosun is restarted, and it is highly recommended that you store the file in version control.

