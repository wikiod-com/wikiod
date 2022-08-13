---
title: "Optimizing Magento 2"
slug: "optimizing-magento-2"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Configurations to optimize
# 1. Enable Flat Categories and Products

One of the top reasons of Magento speed issues with database read speed. To fasten the read speed of the database you should enable **Flat Catalog**. This will minify the number of database joins done when showing products and due to that the MySQL query complexity will be reduced.

Go to backend: **STORES > Configuration > CATALOG > Catalog > Use Flat Catalog Category**  and put **Yes**.

[![enter image description here][1]][1]

# 2. Merge CSS and JS Files

The next step you need to follow is merging and minifying CSS and Javascript files, that means making the web page as light as possible for the fast loading. Please put Magento 2 into **production** mode.

Go to backend: **STORES > Configuration > ADVANCED > Developer > CSS Settings** and put the **Merge CSS Files** and **Minify CSS Files** as **yes**.

[![enter image description here][2]][2]

# 3. Content Delivery Network

CDN, or Content Delivery Network,  is an interconnected system of cache servers that use geographical proximity as criteria for delivering web content and actually helps your visitors to load pages faster as a result.

One of the Magento 2 features is out-of-the-box support of CDN and here’s where you may find set up for it: **STORES > GENERAL > Configuration > Web > Base URLs (Secure)** and input your HTTPS CDN URLs in here and let your customers enjoy fast loading speed.

[![enter image description here][3]][3]

# 4. Caching

In the **System > Cache Management** enable your cache.

[![enter image description here][4]][4]


  [1]: https://i.stack.imgur.com/R07QL.png
  [2]: https://i.stack.imgur.com/7ul1j.png
  [3]: https://i.stack.imgur.com/z2PhX.png
  [4]: https://i.stack.imgur.com/eJ9x0.png

