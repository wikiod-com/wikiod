---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Configuring a log driver in systemd service
    [Service]
    
    # empty exec prevents error "docker.service has more than one ExecStart= setting, which is only allowed for Type=oneshot services. Refusing."
    ExecStart=
    ExecStart=/usr/bin/dockerd -H fd:// --log-driver=syslog

This enables syslog logging for the docker daemon. The file should be created in the appropriate directory with owner root, which typically would be `/etc/systemd/system/docker.service.d` on e.g. Ubuntu 16.04. 

## Overview
Docker's approach to logging is that you construct your containers in such a way, so that logs are written to standard output (console/terminal).

If you already have a container which writes logs to a file, you can redirect it by creating a symbolic link:

    ln -sf /dev/stdout /var/log/nginx/access.log
    ln -sf /dev/stderr /var/log/nginx/error.log

After you've done that you can use various log drivers to put your logs where you need them.


