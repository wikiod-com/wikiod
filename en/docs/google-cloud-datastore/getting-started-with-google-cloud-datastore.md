---
title: "Getting started with google-cloud-datastore"
slug: "getting-started-with-google-cloud-datastore"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Initial Setup
# Google Cloud Project
[Google Cloud Datastore][1] is a NoSQL Database as a Service (DBaaS) that provides developers with a scalable replicated database that is fully managed by Google.

For its initial setup you need to either have an existing [Google Cloud Platform][2] (GCP) project, or create a new one. Cloud Datastore has a free tier, so you do not need to configure billing information at this time. You can select or create a project here: https://console.cloud.google.com/project

Once you have a project, it's ready to go. Navigate to the Datastore tab in the Cloud console and you will be able to manually create entities in your instance.

Cloud Datastore can be access from Compute Engine or Container Engine via a variety of client libraries.

To install the client libraries:

 - C#: `Install-Package Google.Datastore.V1 -Pre`
 - Go: `go get cloud.google.com/go/datastore`
 - Node.js: `npm install --save @google-cloud/datastore`
 - PHP: `composer require google/cloud`
 - Python: `pip install --upgrade google-cloud-datastore`
 - Ruby: `gem install google-cloud-datastore`

For Java:

 - If you use Maven, add the following to your pom.xml file:


    <dependency>
        <groupId>com.google.cloud</groupId>
        <artifactId>google-cloud-datastore</artifactId>
        <version>0.7.0</version>
    </dependency>
 - If you use Gradle, add the following to your dependencies:


    compile group: 'com.google.cloud', name: 'google-cloud-datastore', version: '0.7.0'


More information can be found in the [official documentation][3] for client libraries.


  [1]: https://cloud.google.com/appengine/docs
  [2]: https://cloud.google.com/
  [3]: https://cloud.google.com/datastore/docs/reference/libraries

