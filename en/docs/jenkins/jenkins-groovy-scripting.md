---
title: "Jenkins Groovy Scripting"
slug: "jenkins-groovy-scripting"
draft: false
images: []
weight: 9906
type: docs
toc: true
---

## Create default user
1. Create groovy file by path `$JENKINS_HOME/init.groovy.d/basic-security.groovy`

In Ubuntu 16 Jenkins home directory places in `/var/lib/jenkins`

2. Place in file next code


    #!groovy
    
    import jenkins.model.*
    import hudson.security.*
    
    def instance = Jenkins.getInstance()
    
    def hudsonRealm = new HudsonPrivateSecurityRealm(false)
    
    hudsonRealm.createAccount("admin_name","admin_password")
    instance.setSecurityRealm(hudsonRealm)
    instance.save()



3. Restart Jenkins service
4. After Jenkins starts you need to remove `$JENKINS_HOME/init.groovy.d/basic-security.groovy` file

## Disable Setup Wizard
1. Open Jenkins default config file and add in `JAVA_ARGS` next key `-Djenkins.install.runSetupWizard=false`

In Ubuntu 16 default file places in `/etc/default/jenkins`

2. Create groovy file by path `$JENKINS_HOME/init.groovy.d/basic-security.groovy`

In Ubuntu 16 Jenkins home directory places in `/var/lib/jenkins`

3. Place in file next code

```
#!groovy

import jenkins.model.*
import hudson.util.*;
import jenkins.install.*;

def instance = Jenkins.getInstance()

instance.setInstallState(InstallState.INITIAL_SETUP_COMPLETED)

```

4. Restart Jenkins service
5. After Jenkins starts you need remove `$JENKINS_HOME/init.groovy.d/basic-security.groovy` file

After this Jenkins doesn`t ask you to confirm that you are admin and you will not see plugins install page.

## How to get infromation about Jenkins instance
Open your jenkins instance script console http://yourJenkins:port/script
following is an example for how to get information about this instance. copy the code to the console and click "Run".

    /* This scripts shows how to get basic information about Jenkins instance */
    def jenkins = Jenkins.getInstance()
    println "Jenkins version: ${jenkins.getVersion()}"
    println "Available JDKs: ${jenkins.getInstance().getJDKs()}"
    println "Connected Nodes:"
    jenkins.getNodes().each{ 
      println it.displayName
    }
    println "Configured labels: ${jenkins.getLabels()}"

In this example you will see information about the Jenkins version, JDKs, agents(slaves) and labels.


## How to get information about a Jenkins job
Open your jenkins instance script console http://yourJenkins:port/script
following is an example for how to get information about a sepcific job. copy the code to the console, change the jobName to the required job and click "Run".

    /*This script shows how to get basic information about a job and its builds*/
    def jenkins = Jenkins.getInstance()
    def jobName = "myJob"
    def job = jenkins.getItem(jobName)

    println "Job type: ${job.getClass()}"
    println "Is building: ${job.isBuilding()}"
    println "Is in queue: ${job.isInQueue()}"
    println "Last successfull build: ${job.getLastSuccessfulBuild()}"
    println "Last failed build: ${job.getLastFailedBuild()}"
    println "Last build: ${job.getLastBuild()}"
    println "All builds: ${job.getBuilds().collect{ it.getNumber()}}"

first we get the Jenkins instance object, then using this instance we get the job object (item). from the job object we can get different information such as: is it currently building, is it in the queue, the last build, last build by status, and a lot more.


