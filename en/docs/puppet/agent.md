---
title: "Agent"
slug: "agent"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
 1. puppet agent [--certname NAME] [-D|--daemonize|--no-daemonize]
    [-d|--debug] [--detailed-exitcodes] [--digest DIGEST] [--disable
    [MESSAGE]] [--enable] [--fingerprint] [-h|--help] [-l|--logdest
    syslog|eventlog|FILE|console] [--masterport PORT] [--noop]
    [-o|--onetime] [-t|--test] [-v|--verbose] [-V|--version]
    [-w|--waitforcert SECONDS]

## Trigger
By default the agent is triggered every 30 minutes.
This interval value can be changed from the `puppet.conf` file.

 - Linux-   `/etc/puppet/puppet.conf`
 - Windows - `%PROGRAMDATA%\PuppetLabs\puppet\etc\puppet.conf`

Set the `runinterval` to the wanted interval.

    runinterval=xxx

The agent can be triggered manually with the command:

    puppet agent -t 

## Logging
Puppet agnet logs messages.
You can view this logs here:

**Linux** - `/var/log/puppet/puppet.log`

 **Windows** - view the `Event Viewer` (Control Panel → System and Security → Administrative Tools → Event Viewer)


## What is it?
The puppet agent is a service that runs on the servers.
Once the service is started, The agent will be triggered on background every 30 min (by default).

The agent have 2 main usages:
 - Send server`s facts to the puppet master
 - Receive catalog from the puppet master ans apply it

## Verbose output
Sometimes it is helpful to get more output on puppet agent run.

It is very useful for debugging.

Run puppet agent with `verbose` and `debug` parameters:

 - `debug` - Enable full debugging.
 - `verbose` - Turn on verbose reporting.



    puppet agent -t --verbose --debug

