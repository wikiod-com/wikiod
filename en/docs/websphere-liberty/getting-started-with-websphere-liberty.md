---
title: "Getting started with websphere-liberty"
slug: "getting-started-with-websphere-liberty"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
**Download:** <br>
To set up WebSphere Liberty, download the latest zip from [WASdev.net][1].

<br>**Layout:** <br>
Once you have the zip, extract it to any location on your file system.  The basic layout of a Liberty install is the following:

    wlp/                # WLP_INSTALL_DIR
      bin/              # location of scripts such as 'server'
      dev/              # developer resources (APIs, SPIs, and tools)
      etc/              # global customizations (server.env or jvm.options)
      lib/              # platform runtime environment
      usr/              # user directory
        servers/        # servers directory
          server_name   # directory containing all information for a given server
            server.xml  # (required) primary server configuration file
            apps/       # server applications folder
            dropins/    # server dropin applications folder
            logs/       # server log files

Detailed layout information: [Directory locations and properties][2]


<br>**Creating, starting, and stopping a server:**<br>

| Action | Command |
| ------ | ------ |
| Create a server| `server create myServer` 
| Start a server | `server start myServer`
| Stop a server | `server stop myServer`

Note: The `server` script is located at: `$WLP_INSTALL_DIR/bin/server`


  [1]: https://developer.ibm.com/wasdev/
  [2]: http://www.ibm.com/support/knowledgecenter/SSEQTP_8.5.5/com.ibm.websphere.wlp.iseries.doc/ae/rwlp_dirs.html

