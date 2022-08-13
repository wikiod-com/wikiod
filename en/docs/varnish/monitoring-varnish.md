---
title: "Monitoring Varnish"
slug: "monitoring-varnish"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

Use `varnishstat` to monitor the numeric metrics of a currently running Varnish instance. It's location will differ based on your installation. Running `varnishstat -1` will output all metrics in a simple `grep`-able format.

Other utilities are available for watching varnish current status and logging: `varnishtop`, `varnishlog` etc.

## Client metrics - incoming traffic
Client metrics cover the traffic between the client and the Varnish cache. 

- sess_conn - Cumulative number of connections.
- client_req - Cumulative number of client requests.
- sess_dropped - Dropped connections because of a full queue.


Monitor `sess_conn` and `client_req` to keep track of traffic volume - is it increasing or decreasing, is it spiking etc. Sudden changes might indicate problems.

Monitor `sess_dropped` to see if the cache is dropping any sessions. If so you might need to increase `thread_pool_max`.


    varnishstat -1 | grep "sess_conn\|client_req \|sess_dropped"
    MAIN.sess_conn              62449574         3.38 Sessions accepted
    MAIN.client_req            184697229         9.99 Good client requests received
    MAIN.sess_dropped                  0         0.00 Sessions dropped for thread


## Cache performance
Perhaps the most important performance metric is the hitrate.

Varnish routes it's incoming requests like this:

- Hash, a cacheable request. This might be either `hit` or `miss` depending on the state of the cache.
- Hitpass, a not cacheable request.

A hash with a `miss` and a `hitpass` will be fetched from the server backend and delivered. A hash with a `hit` will be delivered directly from the cache.

Metrics to monitor:

- cache_hit - Number of hashes with a hit in the cache.
- cache_miss - Number of hashes with a miss in the cache.
- cache_hitpass - Number of hitpasses as above.


    varnishstat -1 | grep "cache_hit \|cache_miss \|cache_hitpass"
    MAIN.cache_hit              99032838         5.36 Cache hits
    MAIN.cache_hitpass                 0         0.00 Cache hits for pass
    MAIN.cache_miss             42484195         2.30 Cache misses

Calculate the actual hitrate like this:

    cache_hit / (cache_hit + cache_miss)

In this example the hitrate is 0.7 or 70%. You want to keep this as high as possible. 70% is a decent number. You can improve hitrate by increasing memory and customizing your vcl. Also monitor big changes in your hitrate.

## Monitoring cached objects
You monitor the cached objects to see how often they expire and if they are "nuked". 

- n_expired - Number of expired objects. 
- n_lru_nuked - Last recently used nuked objects. Number of objects nuked (removed) from the cache because of lack of space.


    varnishstat -1 | grep "n_expired\|n_lru_nuked"
    MAIN.n_expired              42220159          .   Number of expired objects
    MAIN.n_lru_nuked              264005          .   Number of LRU nuked objects

The one to watch here is `n_lru_nuked`, if the rate is increasing (the _rate_, not only the number) your cache is pushing out objects faster and faster because of lack of space. You need to increase the cache size.

The `n_expired` is more up to your application. A longer time to live will decrease this number but on the other hand not renew the objects as often. Also the cache might require more size.



## Monitoring threads
You need to keep track of some threads metrics to watch your Varnish Cache. Is it running out of OS resources or is it functioning well.

- threads - Number of threads in all pools.
- threads_created - Number of created threads.
- threads_failed - Number of times Varnish failed to create a thread.
- threads_limited - Number of times Varnish was forced not to create a thread since it was maxed out.
- thread_queue_len - Current queue length. Number of requests waiting for a thread.
- sess_queued - Number of times there wasn't any threads available so a request had to be queued.


    varnishstat -1 | grep "threads\|thread_queue_len\|sess_queued"
    MAIN.threads                     100          .   Total number of threads
    MAIN.threads_limited               1         0.00 Threads hit max
    MAIN.threads_created            3715         0.00 Threads created
    MAIN.threads_destroyed          3615         0.00 Threads destroyed
    MAIN.threads_failed                0         0.00 Thread creation failed
    MAIN.thread_queue_len              0          .   Length of session queue
    MAIN.sess_queued                2505         0.00 Sessions queued for thread

If `thread_queue_len` isn't 0 it means that Varnish is out of resources and have started to queue requests. This will decrease performance of those requests. You need to investigate why. 

Watch also out for `threads_failed`. If this increases it means your server is out of resources somehow.  Increasing numbers in `threads_limited` means you might need to increase your servers `thread_pool_max`.


## Monitoring backend metrics
There are a number of metrics describing the communication between Varnish and it's backends. 

The most important metrics here might be these:

- backend_busy - Number of http 5xx statuses recieved by a backend. With VCL you can configure Varnish to try another backend if this happens.
- backend_fail - Number of times Varnish couldnt connect to the backend. This can have a number of causes (no TCP-connection, long time to first byte, long time between bytes). If this happens your backend isn't healthy.
- backend_unhealthy - Number of times Varnish couldn't "ping" the backend (it didn't respond with a HTTP 200 response.


    varnishstat -1 | grep "backend_"
    MAIN.backend_conn           86913481         4.70 Backend conn. success
    MAIN.backend_unhealthy             0         0.00 Backend conn. not attempted
    MAIN.backend_busy                  0         0.00 Backend conn. too many
    MAIN.backend_fail                  7         0.00 Backend conn. failures
    MAIN.backend_reuse                 0         0.00 Backend conn. reuses
    MAIN.backend_toolate               0         0.00 Backend conn. was closed
    MAIN.backend_recycle               0         0.00 Backend conn. recycles
    MAIN.backend_retry                 0         0.00 Backend conn. retry
    MAIN.backend_req            86961073         4.70 Backend requests made



