---
title: "Silencing and Squelching Alerts"
slug: "silencing-and-squelching-alerts"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Squelching a host
If one does not want to receive any alert for a specific host or service - at least momentarily - one can squelch it.

    alert thisis.down {
      macro = host.mymacro
      template = mytemplate
      $notes = This alert will...
      $metric = "avg:os.service.running{host=*,name=...
      warn = min( a($metric, ...

      squelch = host=sqldev01,flavor=amq
      squelch = host=test01
    }

This alert won't appear in the dashboard for service *amq* on host *sqldev01*, and won't appear at all for any service running on host *test01*.


