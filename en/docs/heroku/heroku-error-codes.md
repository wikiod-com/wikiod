---
title: "Heroku Error Codes"
slug: "heroku-error-codes"
draft: false
images: []
weight: 9888
type: docs
toc: true
---

Whenever your app experiences an error, Heroku will return a standard error page with the HTTP status code 503. To help you debug the underlying error, however, the platform will also add custom error information to your logs. Each type of error gets its own error code, with all HTTP errors starting with the letter H and all runtime errors starting with R. Logging errors start with L.

## Syntax
- H10 - App crashed
- H11 - Backlog too deep
- H12 - Request timeout
- H13 - Connection closed without response
- H14 - No web dynos running
- H15 - Idle connection
- H16 - Redirect to herokuapp.com
- H17 - Poorly formatted HTTP response
- H18 - Server Request Interrupted
- H19 - Backend connection timeout
- H20 - App boot timeout
- H21 - Backend connection refused
- H22 - Connection limit reached
- H23 - Endpoint misconfigured
- H24 - Forced close
- H25 - HTTP Restriction
- H26 - Request Error
- H27 - Client Request Interrupted
- H28 - Client Connection Idle
- H80 - Maintenance mode
- H81 - Blank app
- H82 - Free dyno quota exhausted
- H99 - Platform error
- R10 - Boot timeout
- R12 - Exit timeout
- R13 - Attach error
- R14 - Memory quota exceeded
- R15 - Memory quota vastly exceeded
- R16 – Detached
- R17 - Checksum error
- R99 - Platform error
- L10 - Drain buffer overflow
- L11 - Tail buffer overflow
- L12 - Local buffer overflow
- L13 - Local delivery error
- L14 - Certificate validation error

## H13 - Connection closed without response
This error is thrown when a process in your web dyno accepts a connection, but then closes the socket without writing anything to it.

    2010-10-06T21:51:37-07:00 heroku[router]: at=error code=H13 desc="Connection closed without response" method=GET path="/" host=myapp.herokuapp.com fwd=17.17.17.17 dyno=web.1 connect=3030ms service=9767ms status=503 bytes=0

One example where this might happen is when a Unicorn web server is configured with a timeout shorter than 30s and a request has not been processed by a worker before the timeout happens. In this case, Unicorn closes the connection before any data is written, resulting in an H13.

## H10 - App crashed
A crashed web dyno or a boot timeout on the web dyno will present this error.

    2010-10-06T21:51:04-07:00 heroku[web.1]: State changed from down to starting
    2010-10-06T21:51:07-07:00 app[web.1]: Starting process with command: `bundle exec rails server -p 22020`
    2010-10-06T21:51:09-07:00 app[web.1]: >> Using rails adapter
    2010-10-06T21:51:09-07:00 app[web.1]: Missing the Rails 2.3.5 gem. Please `gem install -v=2.3.5 rails`, update your RAILS_GEM_VERSION setting in config/environment.rb for the Rails version you do have installed, or comment out RAILS_GEM_VERSION to use the latest version installed.
    2010-10-06T21:51:10-07:00 heroku[web.1]: Process exited
    2010-10-06T21:51:12-07:00 heroku[router]: at=error code=H10 desc="App crashed" method=GET path="/" host=myapp.herokuapp.com fwd=17.17.17.17 dyno= connect= service= status=503 bytes=

## H11 - Backlog too deep
When HTTP requests arrive faster than your application can process them, they can form a large backlog on a number of routers. When the backlog on a particular router passes a threshold, the router determines that your application isn’t keeping up with its incoming request volume. You’ll see an H11 error for each incoming request as long as the backlog is over this size. The exact value of this threshold may change depending on various factors, such as the number of dynos in your app, response time for individual requests, and your app’s normal request volume.

    2010-10-06T21:51:07-07:00 heroku[router]: at=error code=H11 desc="Backlog too deep" method=GET path="/" host=myapp.herokuapp.com fwd=17.17.17.17 dyno= connect= service= status=503 bytes=

The solution is to increase your app’s throughput by adding more dynos, tuning your database (for example, adding an index), or making the code itself faster. As always, increasing performance is highly application-specific and requires profiling.

## H12 - Request timeout
An HTTP request took longer than 30 seconds to complete. In the example below, a Rails app takes 37 seconds to render the page; the HTTP router returns a 503 prior to Rails completing its request cycle, but the Rails process continues and the completion message shows after the router message.

    2010-10-06T21:51:07-07:00 app[web.2]: Processing PostController#list (for 75.36.147.245 at 2010-10-06 21:51:07) [GET]
    2010-10-06T21:51:08-07:00 app[web.2]: Rendering template within layouts/application
    2010-10-06T21:51:19-07:00 app[web.2]: Rendering post/list
    2010-10-06T21:51:37-07:00 heroku[router]: at=error code=H12 desc="Request timeout" method=GET path="/" host=myapp.herokuapp.com fwd=17.17.17.17 dyno=web.1 connect=6ms service=30001ms status=503 bytes=0
    2010-10-06T21:51:42-07:00 app[web.2]: Completed in 37000ms (View: 27, DB: 21) | 200 OK [http://myapp.heroku.com/]

This 30-second limit is measured by the router, and includes all time spent in the dyno, including the kernel’s incoming connection queue and the app itself.

## H14 - No web dynos running
This is most likely the result of scaling your web dynos down to 0 dynos. To fix it, scale your web dynos to 1 or more dynos:

    $ heroku ps:scale web=1

Use the heroku ps command to determine the state of your web dynos.

    2010-10-06T21:51:37-07:00 heroku[router]: at=error code=H14 desc="No web processes running" method=GET path="/" host=myapp.herokuapp.com fwd=17.17.17.17 dyno= connect= service= status=503 bytes=

## H15 - Idle connection
The dyno did not send a full response and was terminated due to 55 seconds of inactivity. For example, the response indicated a `Content-Length` of 50 bytes which were not sent in time.

    2010-10-06T21:51:37-07:00 heroku[router]: at=error code=H15 desc="Idle connection" method=GET path="/" host=myapp.herokuapp.com fwd=17.17.17.17 dyno=web.1 connect=1ms service=55449ms status=503 bytes=18

