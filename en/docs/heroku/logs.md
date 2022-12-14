---
title: "Logs"
slug: "logs"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
- $ heroku logs
- $ heroku logs -n 200
- $ heroku logs --tail
- $ heroku logs --dyno router
- $ heroku logs --source app
- $ heroku logs --source app --dyno worker
- $ heroku logs --source app --tail

## Log Filtering
If you only want to fetch logs with a certain source, a certain dyno, or both, you can use the `--source` (or `-s`) and `--dyno` (or `-d`) filtering arguments:

    $ heroku logs --dyno router
    2012-02-07T09:43:06.123456+00:00 heroku[router]: at=info method=GET path="/stylesheets/dev-center/library.css" host=devcenter.heroku.com fwd="204.204.204.204" dyno=web.5 connect=1ms service=18ms status=200 bytes=13
    2012-02-07T09:43:06.123456+00:00 heroku[router]: at=info method=GET path="/articles/bundler" host=devcenter.heroku.com fwd="204.204.204.204" dyno=web.6 connect=1ms service=18ms status=200 bytes=20375
    
    $ heroku logs --source app
    2012-02-07T09:45:47.123456+00:00 app[web.1]: Rendered shared/_search.html.erb (1.0ms)
    2012-02-07T09:45:47.123456+00:00 app[web.1]: Completed 200 OK in 83ms (Views: 48.7ms | ActiveRecord: 32.2ms)
    2012-02-07T09:45:47.123456+00:00 app[worker.1]: [Worker(host:465cf64e-61c8-46d3-b480-362bfd4ecff9 pid:1)] 1 jobs processed at 23.0330 j/s, 0 failed ...
    2012-02-07T09:46:01.123456+00:00 app[web.6]: Started GET "/articles/buildpacks" for 4.1.81.209 at 2012-02-07 09:46:01 +0000
    
    $ heroku logs --source app --dyno worker
    2012-02-07T09:47:59.123456+00:00 app[worker.1]: [Worker(host:260cf64e-61c8-46d3-b480-362bfd4ecff9 pid:1)] Article#record_view_without_delay completed after 0.0221
    2012-02-07T09:47:59.123456+00:00 app[worker.1]: [Worker(host:260cf64e-61c8-46d3-b480-362bfd4ecff9 pid:1)] 5 jobs processed at 31.6842 j/s, 0 failed ...

You can also combine the filtering switches with `--tail` to get a real-time stream of filtered output.

    $ heroku logs --source app --tail

## Types of logs
Heroku aggregates three categories of logs for your app:

- **App logs** - Output from your application. This will include logs generated from within your application, application server and libraries. (Filter: `--source app`)

- **System logs** - Messages about actions taken by the Heroku platform infrastructure on behalf of your app, such as: restarting a crashed process, sleeping or waking a web dyno, or serving an error page due to a problem in your app. (Filter: `--source heroku`)

- **API logs** - Messages about administrative actions taken by you and other developers working on your app, such as: deploying new code, scaling the process formation, or toggling maintenance mode. (Filter: `--source heroku --dyno api`)

## Log format
Each log line is formatted as follows:

    timestamp source[dyno]: message

- **Timestamp** - The date and time recorded at the time the log line was produced by the dyno or component. The timestamp is in the format specified by RFC5424, and includes microsecond precision.

-  **Source** - All of your app???s dynos (web dynos, background workers, cron) have the source, `app`. All of Heroku???s system components (HTTP router, dyno manager) have the source, `heroku`.

- **Dyno** - The name of the dyno or component that wrote the log line. For example, worker #3 appears as `worker.3`, and the Heroku HTTP router appears as `router`.

- **Message** - The content of the log line. Lines generated by dynos that exceed 10000 bytes are split into 10000 byte chunks without extra trailing newlines. Each chunk is submitted as a separate log line.

## View logs
To fetch your logs, use the `heroku logs` command.

    $ heroku logs

The logs command retrieves 100 log lines by default. You can specify the number of log lines to retrieve (up to a maximum of 1,500 lines) by using the `--num` (or `-n`) option.

    $ heroku logs -n 200

# Real-time tail

Similar to `tail -f`, real-time tail displays recent logs and leaves the session open for real-time logs to stream in. By viewing a live stream of logs from your app, you can gain insight into the behavior of your live application and debug current problems.
You can tail your logs using `--tail` (or `-t`).

    $ heroku logs --tail

When you are done, press Ctrl+C to return to the prompt.

