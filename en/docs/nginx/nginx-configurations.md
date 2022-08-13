---
title: "Nginx Configurations"
slug: "nginx-configurations"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Test NGINX configuration file
You can check for syntax errors and referenced files in an NGINX configuration file before running it with:

    nginx -t

Alternatively you can run service script 

    service nginx configtest

While both of these commands will tell you if your new nginx configuration is ok [without killing your current instance]. Configtest uses the running service and tells you if it passes or fails the check whereas nginx -t will not only check the config but print any info, warning as well as error messages.

Source: http://devget.net/nginxapache/nginx-configtest-vs-nginx-t/

## config changes without restart
`nginx -s reload`

## Configuration File’s Structure
nginx consists of modules which are controlled by directives specified in the configuration file.  

# Simple Directives

A simple directive consists of the name and parameters separated by spaces and ends with a semicolon (;). 

# Block Directive

A block directive has the same structure as a simple directive, but instead of the semicolon it ends with a set of additional instructions surrounded by braces ({ and }). 

# Context 

If a block directive can have other directives inside braces, it is called a context (examples: events, http, server, and location).

Directives placed in the configuration file outside of any contexts are considered to be in the main context. The events and http directives reside in the main context, server in http, and location in server.

# Comment

The rest of a line after the # sign is considered a comment.

## Specify configuration file to load
    nginx -c <file name>

Start NGINX with an explicit configuration file.

## Logging to Syslog, incl. mapping of HTTP-Codes to Syslog severities.
Paste this snippet somewhere in the `http {}` Block; or place it in it's own file in the `/etc/nginx/conf.d/` folder. Also see the [official docs][1] for logging to syslog.
 
    # 
    # Access Log
    # 
    log_format fmt_syslog '[$time_local] $status $remote_addr $http_host "$request" $body_bytes_sent $request_time "$http_user_agent" $remote_user';
    map $status $log_is_error { "~^5\d\d"     1; default 0; }
    map $status $log_is_warn  { "~^4[0-8]{2}" 1; default 0; }
    map $status $log_is_info  { "~^[1-3]\d\d" 1; default 0; }
    access_log syslog:server=unix:/run/systemd/journal/syslog,nohostname,facility=local2,severity=error fmt_syslog if=$log_is_error;
    access_log syslog:server=unix:/run/systemd/journal/syslog,nohostname,facility=local2,severity=warn  fmt_syslog if=$log_is_warn;
    access_log syslog:server=unix:/run/systemd/journal/syslog,nohostname,facility=local2,severity=info  fmt_syslog if=$log_is_info;
    #
    # Error Log
    #
    error_log syslog:server=unix:/run/systemd/journal/syslog,nohostname,facility=local2 error;

This example assumes rsyslog (or similar) is listening on Socket `/run/systemd/journal/syslog` - as it's default on Debian 8 when journald has activated [ForwardToSyslog][2]. Using this socket, you bypass journald. If that socket is not available try `/dev/log` instead.

Feel free to use another facility instead of local2. You may also change the [log_format][3] to suit your needs.


  [1]: http://nginx.org/en/docs/syslog.html
  [2]: https://www.freedesktop.org/software/systemd/man/journald.conf.html
  [3]: http://nginx.org/en/docs/http/ngx_http_log_module.html#log_format

## Limit request methods
A usual Website just needs 3 HTTP Methods: `GET`, `HEAD` and `POST`. Block all other Methods by using [limit_except][1]:

    location / {
        [...]
        # Note: GET includes HEAD
        limit_except GET POST {
            deny  all;
        }
        [...]
    } 


  [1]: http://nginx.org/en/docs/http/ngx_http_core_module.html#limit_except

