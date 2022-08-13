---
title: "Starting a local JIRA test instance"
slug: "starting-a-local-jira-test-instance"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

For starting a JIRA test instance on a local machine, the Atlassian-SDK is the way to go. This is useful for testing JIRA itself, developing and debugging JIRA plugins. The SDK is available for Windows, Linux and Mac.

See the [installation guide for the Atlassian SDK](https://developer.atlassian.com/docs/getting-started/set-up-the-atlassian-plugin-sdk-and-build-a). For running the Atlassian SDK properly, the Oracle Java SE Development Kit 8 (JDK) has to be installed - OpendJDK does not work!




## Parameters
| Parameter (short)| Description |
| ------ | ------ | ---- |
| --version (-v)   | Version of the application to run *(default: latest version)*. |
| --http-port (-p)    | HTTP port for the servlet container. You may need to change this if you already have a process listed for the default port, such as when you want to bring up two instances of JIRA. *(default: 2990)*|
| --context-path | The application context path. You will need to include the leading forward slash. For example, if your application is running at http&#58;//localhost:2990/jira then you should enter /jira. To run your application in the root web application context (eg. http&#58;//localhost:2990), then you should enter ROOT. *(default: /jira)*|
| --server   | Host name of the application server. *(default: localhost)*|
| --product   | The application to launch. In this case use 'jira'. You may also start instances of other Atlassian products (e.g. Confluence)|


## Starting a local JIRA test instance
After installing the SDK, `atlas-run-standalone` starts a JIRA instance with the latest released version.

    atlas-run-standalone --product jira

The instance is available `http://localhost:2990/jira`

The created instance is for testing only. It provides an other directory structure as production installations. It has a license for 10 users.

## Customize the local JIRA instance
It is possible to customize the JIRA version, the port or the context path.

    atlas-run-standalone --product jira --version 6.0 --http-port 1337 --context-path issues

The JIRA 6 instance is available under `http://localhost:1337/issues`


There are more parameters that can be set for running `atlas-run-standalone`. 
For full list of parameters, visit [atlas-run-standalone documentation][1]

  [1]: https://developer.atlassian.com/docs/developer-tools/working-with-the-sdk/command-reference/atlas-run-standalone

