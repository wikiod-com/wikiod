---
title: "Getting started with google-app-engine"
slug: "getting-started-with-google-app-engine"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup
[Google AppEngine][6] (GAE) is a Platform as a Service (PaaS) that provides the ability to deploy applications at "Google Scale".  It is one of the many services on [Google Cloud Platform][1] (GCP). Developers can integrate other services such as [Google Cloud Storage][4] (GCS) and [Google Cloud SQL][5] on GCP easily. Developers can write a set of code that runs locally and can easily be deployed on [Google Cloud Platform][1].

A birds eye view of getting started with AppEngine includes the following:

 - [Install the SDK][2] for your preferred language (Go, Python, Java, PHP, Node.js(in Beta))
 - Use the SDK to scaffold an application & develop locally
 - Deploy the same code that runs locally, to a scalable runtime environment

The AppEngine SDK can also be installed using the Google Cloud SDK:

- [Install the Google Cloud SDK][7]
- [Initialize the Google Cloud SDK](https://cloud.google.com/sdk/docs/initializing)
- [Authorize the Google Cloud SDK](https://cloud.google.com/sdk/docs/authorizing)
- Install the GAE components. For python user, 
    ```
    gcloud components install app-engine-python app-engine-python-extras
    ```
  and for go user,
    ```
    gcloud components install app-engine-go
    ```
  For other languages, use `gcloud components list` to get the list of installed and available components.
- After awhile, the GAE SDK will be installed. 


Other useful links:

 - Formal [Google Documentation][3]


  [1]: https://cloud.google.com/
  [2]: https://cloud.google.com/appengine/downloads
  [3]: https://cloud.google.com/appengine/docs
  [4]: https://cloud.google.com/storage/
  [5]: https://cloud.google.com/sql/
  [6]: https://cloud.google.com/appengine/
  [7]: https://cloud.google.com/sdk/

