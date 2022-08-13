---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Basic example
# Syntax
```
Syntax:  log_format name string ...;
Syntax:  access_log path [format [buffer=size] [gzip[=level]] [flush=time] [if=condition]];
access_log off;
```

Using them together

```
log_format compression '$remote_addr - $remote_user [$time_local] '
                       '"$request" $status $bytes_sent '
                       '"$http_referer" "$http_user_agent" "$gzip_ratio"';

access_log /spool/logs/nginx-access.log compression buffer=32k;
error_log /spool/logs/nginx-error.log;
```

## Reopen log files
As a root user run:

    nginx -s reopen

## Avoid logging for favicon.ico and robots.txt
    location = /favicon.ico {
        log_not_found off;
        access_log off;
    }

    location = /robots.txt {
        allow all;
        log_not_found off;
        access_log off;
    }

