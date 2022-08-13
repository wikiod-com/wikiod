---
title: "Google App Engine"
slug: "google-app-engine"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Google App Engine(GAE) is a Platform as a Service(PaaS) offering on Google Cloud Platform, which abstracts away the infrastructure so that you focus on your web application code.  App Engine handles both automatic scaling up and down of instances on-demand for your web application based on the number of requests.

App Engine is available in 2 types of environments - Standard and Flexible and supports the following programming languages: Go, Java, Python, PHP, Node.JS, Ruby, and .NET.

## app.yaml for Php app on Flexible environment
    runtime: php
    vm: true
    api_version: 1
    
    runtime_config:
      document_root: web

## Installing gcloud cli
This is how you install the `gcloud` command-line tool, which is a part of the Google Cloud SDK.

    $ curl https://sdk.cloud.google.com | bash




## Login and Initialize
Authorize yourself, you will be navigated to Google Sign In page.

    $ gcloud auth login

Initialize or reinitialize the `gcloud`, you can set your configurations here.

    $ gcloud init

Display information about the current `gcloud` environment.

    $ gcloud info

Display the list of active SDK configurations.

    $ gcloud config list

Display the list of accounts whoes credentials are stored on the local system.

    $ gcloud auth list

Display the help options.

    $ gcloud help

## Connect to Cloud SQL instance using Php
Create a Cloud SQL instance

     $dsn = "/cloudsql/PROJECT:REGION:INSTANCE;dbname=DATABASE";
     $user = "USER";
     $password = "PASSWORD";
     $db = new PDO($dsn, $user, $password); //Whatever is your favorite MySQL connection method

The important part here is `/cloudsql/PROJECT:REGION:INSTANCE;`. Here we are connecting to the Cloud SQL instance via a Unix socket. You can find this in instance properties as `Instance connection name`.

## Write to Cloud Storage
App Engine does not allow writing to files, instead write to and read from Cloud Storage.

Write to Cloud Storage

    $file = 'gs://<your-bucket>/hello.txt';
    file_put_contents($file, 'hello world');

Read from Cloud Storage

    $contents = file_get_contents($file);
    var_dump($contents); 



## Logging and Log Monitoring
Simply use PHP’s standard syslog function to write logs

    syslog(LOG_INFO, "Authorized access");
    syslog(LOG_WARNING, "Unauthorized access");

You can see the logs from "Stackdriver Logging" ( https://console.cloud.google.com/logs )

