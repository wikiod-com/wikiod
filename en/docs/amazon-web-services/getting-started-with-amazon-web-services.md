---
title: "Getting started with amazon-web-services"
slug: "getting-started-with-amazon-web-services"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Before it is too late
Tips & Tricks to avoid nasty situations

**EC2 Instances and EBS** 

- Set IAM Roles. 

Unlike tags, the IAM Role is **set once and for all on EC2 instanciation** ([even after 4 years][1]) ! Try to identify and categorize beforehand your instances so you can give an them appropriate IAM roles. IAM Roles are a nice way to identify your machines, it will let amazon automatically store Instance Profile credentials safely in your machines, and you will be easily able to give extra privileges.

Consider the following situation where you have Database servers, and you realize you want to monitor memory/disk usage. Amazon CloudWatch does not provide this metric out-of-the-box, and You'll need to set up extra privileges to send custom data to CloudWatch. If you have an IAM "Database" Role, you can easily attach new policies to your existing Database instances to let them send Memory reports to CloudWatch. No IAM Roles ? You have to recreate your Database instances, or give them permission individually.

 - Beware of snapshot integrity

Amazon lets you snapshot EBS volumes, however in case you use several volumes on the same machine (in RAID configuration, multiple EBS volumes for your database), it is impossible to guarantee the integrity of those snapshots, which may happen at different times on the different EBS volumes.

Always ensure no data is written (stop the VM, or use application specific code (eg `db.fsyncLock()`) to ensure no data is written during the snapshot. 

**CloudWatch**

 - Use Amazon Cloudwatch + SNS alerts on top of your application error notifiers

Create alerts for anormal behavior of your machines, and configure to send notifications via Amazon SNS (eg email addresses) in case of problems. Having exception notifiers on your application won't help if your server cannot even be pinged. On the other hand, apart from 500 error codes Amazon has little information on your application and how it is supposed to work, you should consider adding application-specific health monitoring.

  [1]: https://forums.aws.amazon.com/thread.jspa?threadID=97487

