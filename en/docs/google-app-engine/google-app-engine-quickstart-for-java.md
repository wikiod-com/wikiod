---
title: "Google App Engine Quickstart for Java"
slug: "google-app-engine-quickstart-for-java"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Before you begin
Before running this sample, you must:

Download and install the Java SE Development Kit (JDK):

[Download JDK][1]

Download [Apache Maven][2] version 3.3.9 or greater:

Install and configure Maven for your local development environment.


  [1]: http://www.oracle.com/technetwork/java/javase/downloads/index.html
  [2]: https://maven.apache.org/download.cgi

## Download the Hello World app
We've created a simple Hello World app for Java so you can quickly get a feel for deploying an app to Google Cloud Platform. Follow these steps to download Hello World to your local machine.

Clone the Hello World sample app repository to your local machine:

    git clone https://github.com/GoogleCloudPlatform/java-docs-samples.git

Go to the directory that contains the sample code:

    cd java-docs-samples/appengine/helloworld

In the resulting `helloworld` files you'll find the `src` directory for a package called `com.example.appengine.helloworld` that implements a simple `HTTPServlet`.

Alternatively, you can [download the sample][1] as a zip file and extract it.


  [1]: https://github.com/GoogleCloudPlatform/java-docs-samples/archive/master.zip

## Test the application
Test the application using the development web server, which is included with the App Engine SDK.

 1. Run the following Maven command from within your `helloworld` directory, to compile your app and start the development web server:

    mvn appengine:devserver

   The web server is now listening for requests on port 8080. 

 2. Visit http://localhost:8080/ in your web browser to see the app in action.



For more information about running the development web server, see the [Java Development Server reference][1].


  [1]: https://cloud.google.com/appengine/docs/java/tools/devserver

## Make a change
You can leave the web server running while you develop your application. When you make a change, use the `mvn clean package` command to build and update your app.

 1. Try it now: Leave the web server running, then edit
    `HelloServlet.java` to change `Hello, world` to something else. 
 2. Run `mvn clean package`, then reload http://localhost:8080/ to see the
    results.



## Deploy your app

To deploy your app to App Engine, you will need to register a project to create your project ID, which will determine the URL for the app.

 1. In the Cloud Platform Console, go to the Projects page and select or create a new project.
 2. Note the project ID you created above, and enter it in src/main/webapp/WEB-INF/appengine-web.xml. You can also set the app version in this file.
 3. Upload your application to Google App Engine by invoking the following command.

    mvn appengine:update

 4. Your app is now deployed and ready to serve traffic at http://<YOUR_PROJECT_ID>.appspot.com/.


