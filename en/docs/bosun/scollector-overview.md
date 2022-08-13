---
title: "Scollector Overview"
slug: "scollector-overview"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

[Scollector][1] is a monitoring agent that can be used to send metrics to Bosun or any system that accepts OpenTSDB style metrics. It is modelled after [OpenTSDB's tcollector](https://github.com/OpenTSDB/tcollector) data collection framework but is written in Go and compiled into a single binary. One of the design goals is to auto-detect services so that metrics will be sent with minimal or no configuration needed. You also can create [external collectors][2] that generate metrics using a script or executable and use Scollector to queue and send the metrics to the server.

You are NOT required to use Scollector when using Bosun, as you can also send metrics directly to the /api/put route, use another monitoring agent, or use a different backend like Graphite, InfluxDB, or ElasticSearch.


  [1]: http://bosun.org/scollector/
  [2]: https://www.wikiod.com/bosun/scollector-external-collectors

## Setup with sample scollector.toml file
Scollector binaries for Windows, Mac, and Linux are available from the [Bosun release page](https://github.com/bosun-monitor/bosun/releases) and can be saved to **/opt/scollector/** or **C:\\Program Files\\scollector\\**. The [Scollector configuration file](http://godoc.org/bosun.org/cmd/scollector) uses [TOML v0.2.0](https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.2.0.md) to specify various settings and defaults to being named scollector.toml in the same folder as the binary. The configuration file is optional and only required if you need to override a default value or include settings to activate a specific collector.

```
#Where to send metrics. If omitted the default is bosun:80. 
#Config file setting can also be overridden using -h bosunhostname on command line 
Host = "mybosunserver.example.com:8080"

#Optional folder where to find external collector scripts/binaries
ColDir  = 'C:\Program Files\scollector\collectors'

#Number of data points to include in each batch. Default is 500, should be set higher if you are sending a lot of metrics.
BatchSize = 5000

```

You can then either install Scollector as a service or just run it manually via:

```
#Override default configuration file location
scollector -conf /path/to/myconfig.toml

#List all built-in collectors
scollector -l

#-p will print metrics to the screen instead of sending to Bosun. 
#-f "..." will only run specific collectors. Add DisableSelf = true to toml file to exclude scollector.* self metrics
scollector -p -f "c_cpu_windows,c_network_"
```


## Running Scollector as a service
On Windows you can install Scollector as a service using the -winsvc="install" flag. On Mac and Linux you must manually create a service or init script. For example here is a basic systemd unit file:

```
#Scollector unit file saved to /etc/systemd/system/scollector.service
[Unit]
Description=Scollector Service
After=network.target

[Service]
Type=simple
User=root
ExecStart=/opt/scollector/scollector -h mybosunserver.example.com
Restart=on-abort

[Install]
WantedBy=multi-user.target

```

