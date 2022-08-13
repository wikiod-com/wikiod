---
title: "Deploy Ionic as a website"
slug: "deploy-ionic-as-a-website"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Simply copy the www folder to your web server
Ionic 1.2 officially [supports deployment as a website](http://www.nikola-breznjak.com/blog/ionic/ionic-1-2-is-out/)

If you're not using any Cordova plugins then there is no problem (if you really wish to) to upload the contents of the `www` folder to your server, and woila - you'll have the same app.

However, it is important to note that Ionic 1 never intended for such a use, and the users of your "website" will have to have the newest browser in order to see the "website" correctly (not broken down due to some feature that Ionic is using in either CSS or HTML that some older browsers do not support).

