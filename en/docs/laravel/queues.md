---
title: "Queues"
slug: "queues"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Queues allow your application to reserve bits of work that are time consuming to be handled by a background process.

## Use-cases
For example, if you are sending an email to a customer after starting a task, it's best to immediately redirect the user to the next page while queuing the email to be sent in the background. This will speed up the load time for the next page, since sending an email can sometimes take several seconds or longer.

Another example would be updating an inventory system after a customer checks out with their order. Rather than waiting for the API calls to complete, which may take several seconds, you can immediately redirect user to the checkout success page while queuing the API calls to happen in the background.

## Queue Driver Configuration
Each of Laravel's queue drivers are configured from the `config/queue.php` file. A queue driver is the handler for managing how to run a queued job, identifying whether the jobs succeeded or failed, and trying the job again if configured to do so.

Out of the box, Laravel supports the following queue drivers:

### `sync`

Sync, or synchronous, is the default queue driver which runs a queued job within your existing process. With this driver enabled, you effectively have no queue as the queued job runs immediately. This is useful for local or testing purposes, but clearly not recommended for production as it removes the performance benefit from setting up your queue.

### `database`

This driver stores queued jobs in the database. Before enabling this driver, you will need to create database tables to store your queued and failed jobs:

    php artisan queue:table
    php artisan migrate

### `sqs`

This queue driver uses [Amazon's Simple Queue Service][1] to manage queued jobs. Before enabling this job you must install the following composer package: `aws/aws-sdk-php ~3.0`

Also note that if you plan to use delays for queued jobs, Amazon SQS only supports a maximum delay of 15 minutes.

### `iron`

This queue drivers uses [Iron][2] to manage queued jobs.

### `redis`

This queue driver uses an instance of [Redis][3] to manage queued jobs. Before using this queue driver, you will need to configure a copy of Redis and install the following composer dependency: `predis/predis ~1.0`

### `beanstalkd`

This queue driver uses an instance of [Beanstalk][4] to manage queued jobs. Before using this queue driver, you will need to configure a copy of Beanstalk and install the following composer dependency: `pda/pheanstalk ~3.0`

### `null`

Specifying null as your queue driver will discard any queued jobs.

  [1]: https://aws.amazon.com/sqs/
  [2]: https://www.iron.io/
  [3]: http://redis.io/
  [4]: http://kr.github.io/beanstalkd/

