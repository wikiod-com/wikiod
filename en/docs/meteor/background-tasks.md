---
title: "Background tasks"
slug: "background-tasks"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

The package **cron-tick** is a very simple package for background tasks but it does not support multiple processes, if you run your app in multiple processes (or containers) use **percolate:synced-cron** instead.

## Simple cron
Use the package **percolate:synced-cron**

Define a job:

     SyncedCron.add({
      name: 'Find new matches for a saved user filter and send alerts',
      schedule: function(parser) {
        // parser is a later.parse object
        return parser.text('every 10 minutes');
      },
      job: function() {
        user.alerts.map(a => a.findMatchesAndAlert());
      }
    });

Starting up your defined jobs:

    SyncedCron.start();

It supports syncronizing jobs between multiple processes, like Galaxy with more than 1 container.

