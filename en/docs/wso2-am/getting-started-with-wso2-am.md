---
title: "Getting started with wso2-am"
slug: "getting-started-with-wso2-am"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Running in stand alone mode
This example shows how you can install and run WSO2 API Manager in your machine.

For this example API Manager 2.1.0 version is used.
You can install WSO2 API Manager in two ways.

 1. Download the API Manager product from [product website.][1] 
 2. Checkout the source code from GitHub, build the source code and get the product created.

**How to install the product downloaded from the product website ?**

Prerequisites : 

 1. **To run WSO2 API Manager server** in your machine you want Oracle JRE 1.7.* or 1.8.* installed in your machine. (Do not use Open JDK)
 2. A web browser

**Steps :** 

 1. Extract the downloaded zip file.

 2. Setting environment : 

**If you are on Ubuntu, Solaris or OSX**, 

Open .bashrc file in your home directory via terminal and set the JAVA_HOME.

e.g. 

 `export JAVA_HOME=/usr/lib/jvm/java-8-oracle`

 `export PATH=PATH=${JAVA_HOME}/bin:${PATH}`

**If you are on Windows**, 

Go to Environment Variables and {add a System variable](http://www.wikihow.com/Set-Java-Home) as JAVA_HOME and give the path of your java installation directory to it.
Update the user variable PATH with java bin directory as well.

**Running the Product**

*To run the product in Windows :*

In typical environment, use the following command. 

    wso2server.bat --run

*To run the product in Linux and OS X :*

    sh wso2server.sh
 
To start and stop the server in background mode : 

    sh wso2server.sh start 
    sh wso2server.sh stop

Type -help to check additional startup commands that you can use.

After starting the server, you will be able to access following three components.

WSO2 Management Console : **https://<IP_address>:9443/carbon**

WSO2  API Publisher : **https://<IP_address>:9443/publisher**

WSO2 API Store : **https://<IP_address>:9443/store**

Now let's see how we can build the product from source code.

**How to build the product from source code ?**

Prerequisites : 

To build the source code of WSO2 API Manager, you need following software to be installed in your machine.

1. Oracle Java SE Development Kit JDK 1.7.*/1.8.*
2. Apache ActiveMQ JMS Provider 5.5.0 or later
3. Apache Maven 3.0.*
4. Apache Ant 1.7.0 or later
5. A web browser

To build the product from source code, we need following git repositories to be cloned into your machine and checkout the denoted tags.

https://github.com/wso2/carbon-apimgt.git   ---- tag v6.1.66

https://github.com/wso2/product-apim.git     ---- tag v2.1.0

Then first build carbon-apimgt repository with `mvn clean install` command.
After successfully finishing it, build the product-apim repository with the same command.

If you do not want to run the testes use the following -D property to skip the tests.

    mvn clean install -Dmanven.test.skip=true

After successfully building the product-apim repository, you can find the product as a zip file inside <product-apim>/modules/distribution/prosuct/target directory.

You can extract the zip file and use as described in the above in this documentation like what we have downloaded from the product website.


















   


     


  [1]: http://wso2.com/api-management/#iBottom

