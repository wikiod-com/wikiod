---
title: "Getting started with aem"
slug: "getting-started-with-aem"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
AEM can be installed as a standalone executable JAR file or through web application servers, such as JBoss and WebSphere, as a WAR file.

# Prerequisites

AEM 6.2 needs at minimum the following in order to run

- Java Runtime Environment (JRE) 1.8x (64bit)
- 5GB of free disk space for installation
- 2GB of RAM

# Standalone installation

The standalone installation is the simplest. It only requires quickstart jar file. This is provided by Adobe to you or your company.

Simply double click on the jar file and AEM will start. This might take a while on the initial install (~10 minutes).

Once the initial setup has been completed, a browser window will open (`http://localhost:4502`). You can login using the default admin credentials (user: `admin`/ pass: `admin`). The initial login will prompt you to enter the license details.

## Port Number ##
By default, AEM will be accessible through HTTP at port 4502.  If port 4502 is unavailable, it will be set to one of these ports (in order of preference):
 
1. 4502
2. 8080
3. 8081
4. 8082
5. 8083
6. 8084
7. 8085
8. 8888
9. 9362
10. Random number

To set the port to a different port number, there are two options:
1. **Using the -port option through the command-line** (`java -jar aem-quickstart.jar -p 6754`)
2. **Rename the file** so that it includes the port number.  This has very specific rules
   * The file must start with `cq` 
   * the port number must be 4 or 5 digits and must come after a dash (ex. `cq5-author-p4502.jar`, `cq5-publish-p4503.jar`)
   * If there are any other digits in the filename, the port number needs to prefixed with -p (ex. `cq5-author-p4502.jar`, `cq5-publish-p6754.jar`)

## Setting Run Modes ##
Run modes are identifiers that allow to differentiate AEM instances (e.g. development, test, production, authoring, publish).  Run modes for an instance can be configured by (in order of resolution):
1. **sling.properties** - change the `sling.run.modes` property in `<cq-installation-dir>/crx-quickstart/conf/sling.properties`
2. **Using the -r switch in the command-line** - when starting the standalone JAR include the switch `-r <runmode>` (ex. `java -jar cq-publish-p6754.jar -r publish`) 
3. **system properties (or -D switch)** - Set a property in the start script (`-Dsling.run.modes=test,publish,production`) 
4. **Changing the JAR filename** - Can be used to activate `author` or `publish` run modes using the following template `cq5-<run-mode>-p<port-number>.jar` (ex. `cq-publish-p6754.jar`)

# Installation with an Application Server

AEM can be deployed in application servers such as `Tomcat`, `JBoss` and `Websphere`. You simple need to deploy a `war` file provided to you by Adobe.

## Setting Run Modes ##
Set the `sling.run.modes` property in `WEB-INF/web.xml`

