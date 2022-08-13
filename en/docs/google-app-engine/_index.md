---
title : google-app-engine Tutorial
slug : google-app-engine-tutorial
weight : 9913
draft : false
images : []
type : docs
---

There are two ways to get the GAE SDK (the standalone GAE SDK vs Google Cloud SDK `gcloud`). There are slight differences when the deploying the app using `gcloud`. If you are using `gcloud`, you can use `gcloud app deploy ~/my_app/app.yaml`. The behaviour is different from using the old `appcfg.py`. If you prefer using `appcfg.py`, you will find that it is not available. It is because for some reasons, Google decided to hide it from the developer. However, the `appcfg.py` is, in fact, installed in the directory `google-cloud-sdk/platform/google_appengine/`. Other useful scripts such as `bulkloader.py` are there as well. 

