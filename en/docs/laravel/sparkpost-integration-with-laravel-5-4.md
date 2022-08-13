---
title: "Sparkpost integration with Laravel 5.4"
slug: "sparkpost-integration-with-laravel-54"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Laravel 5.4 comes preinstalled with sparkpost api lib. Sparkpost lib requires secret key which one can find from their sparkpost account.

## SAMPLE .env file data
To successfully create a sparkpost email api setup, add the below details to env file and your application will be good to start sending emails.<br><br>
MAIL_DRIVER=sparkpost<br>
SPARKPOST_SECRET=<sparkpost secret key><br><br>
NOTE: The above details does not give you the code written in controller which has the business logic to send emails using laravels Mail::send function.

