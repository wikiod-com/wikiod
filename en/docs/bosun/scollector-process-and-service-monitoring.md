---
title: "Scollector Process and Service Monitoring"
slug: "scollector-process-and-service-monitoring"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

Scollector can be used to monitor [processes and services](http://bosun.org/scollector/process-monitoring) in Windows and Linux. Some processes like IIS application pools are monitored automatically, but usually you need to specify which processes and services you want to monitor.

## Linux process and systemd service monitoring
Scollector will [monitor Linux processes](http://bosun.org/scollector/process-monitoring#linux) specified in the configuration file.

```
[[Process]]
  Command = "/opt/bosun/bosun"
  Name = "bosun"

[[Process]]
  Command = "ruby"
  Name = "puppet-agent"
  Args = "puppet"

[[Process]]
  Command = "/haproxy$"
  Name = "haproxy-t1"
  Args = "/etc/haproxy-t1/haproxy-t1.cfg"

[[Process]]
  Command = '/usr/bin/redis-server \*:16389'
  Name = "redis-bosun-dev"
  IncludeCount = true
```

Scollector can also use the D-Bus API to determine the state of [services managed by systemd](http://bosun.org/scollector/process-monitoring#systemd-services) and specified in the configuration file.

```
[[SystemdService]]
  Name = "^(puppet|redis-.*|keepalived|haproxy-t.*)$"
  WatchProc = false

[[SystemdService]]
  Name = "^(scollector|memcached)$"
  WatchProc = true
```

## Windows .NET process monitoring
Scollector can also monitor any Windows [processes using the .NET framework](http://bosun.org/scollector/process-monitoring#net-processes). If no ProcessDotNet settings are specified it will default to just monitoring the w3wp worker processes for IIS. You can specify which applications to monitor in the configuration file.

```
[[ProcessDotNet]]
  Name = "^w3wp"

[[ProcessDotNet]]
  Name = "LINQPad"
```

Matching process will be monitored under the `dotnet.*` metrics, and if there is more than one matching process they will be assigned incrementing id tag values starting at 1. Where possible the w3wp names will be changed to match the iis_pool-names used for process monitoring.

## Monitoring Docker Containers
Scollector has built in support for using [cAdvisor][1] to generate **container.*** metrics in Bosun for each Docker container on a host. To get started you will need to start a new container on each docker host:

    docker run --name cadvisor --restart=always -d -p 8080:8080 google/cadvisor

And then from an external source poll for metrics using scollector with the Cadvisor configuration option. If you are using Kubernetes to manage containers you may also want to use the TagOverride option to override the `docker_id` tags (shorten to 12 chars), add a `container_name` and `pod_name` tag, and remove the `docker_name` and `name` tag:

    [[Cadvisor]]
      URL = "http://mydockerhost01:8080"
    
    [[Cadvisor]]
      URL = "http://mydockerhost02:8080"
    
    #Override tags for Kubernetes containers
    [[TagOverride]]
      CollectorExpr = "cadvisor"
      [TagOverride.MatchedTags]
        docker_name = 'k8s_(?P<container_name>[^\.]+)\.[0-9a-z]+_(?P<pod_name>[^-]+)'
        docker_id = '^(?P<docker_id>.{12})'
      [TagOverride.Tags]
        docker_name = ''
        name = ''

You may also want to send the metrics to a test instance of Bosun (maybe using the [Bosun Docker Container][2]) to verify the metrics look correct before sending them to a production Bosun instance (hard to clean up data after it is sent).


  [1]: https://github.com/google/cadvisor
  [2]: https://www.wikiod.com/bosun/getting-started-with-bosun#Docker Quick Start

## Windows proccess and service monitoring
Scollector will monitor any [Windows processes or services](http://bosun.org/scollector/process-monitoring#windows) specified in the configuration file.

```
[[Process]]
  Name = "^scollector"

[[Process]]
  Name = "^chrome"

[[Process]]
  Name = "^(MSSQLSERVER|SQLSERVERAGENT)$"
```

