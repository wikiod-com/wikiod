---
title: "Drupal cache and performace"
slug: "drupal-cache-and-performace"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Cache has been used for the site or system to improve the content delivery fast for the end-users. This topic is created to explore about Drupal inbuilt caching mechanism and provide info how to use it. We need to explore Drupal's inbuilt caching feature with the external contributed modules like Varnish, Memcache, Authcache, File cache etc. those are available to improve the site performance. You can found the best suited example and caching options to use with your site in this topic.

## Enable Drupal site and block cache
Drupal itself provide good caching options to increase the page speed and serve pages fast to end users. Caches are used to improve the performance of your Drupal site. But it also has a drawback that sometimes it could lead the "stale" data. This means, sometimes, the system may start to serve the old pages from the cache.

How to enable Drupal site and block cache on various Drupal versions? See below for answers:
Drupal 6:
1. Go to Administer -> Site Configuration -> Performance.
2. Turn on cache options
3. Enable the JS/CSS files aggregation and save it.

Drupal 7:
1. Go to Administer -> Config -> Development -> Performance.
2. Turn on cache options
3. Enable the JS/CSS files aggregation and save it.


