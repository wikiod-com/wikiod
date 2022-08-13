---
title: "Cron basics"
slug: "cron-basics"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Cron is a task scheduler daemon which runs scheduled tasks at certain intervals. Cron uses a configuration file called crontab, also known as cron table, to manage the scheduling process.

## Create Cron Job
Crontab contains cron jobs, each related to a specific task. Cron jobs are composed of two parts, the cron expression, and a shell command to be run:

> \* * * * * command/to/run

Each field in the above expression `* * * * *` is an option for setting the schedule frequency. It is composed of minute, hour, day of month, month and day of week in order of the placement. The asterisk symbol refers to all possible values for the respective field. As a result, the above cron job will be run every minute in the day.

The following cron job is executed at **12:30** every day:
> 30 12 * * * command/to/run

