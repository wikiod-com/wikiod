---
title: "Getting started with jenkins"
slug: "getting-started-with-jenkins"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setting up Nginx Proxy
Natively, Jenkins runs on port 8080. We can establish a proxy from port 80 -> 8080 so Jenkins can be accessed via:

    http://<url>.com

instead of the default

    http://<url>.com:8080


Begin by installing Nginx.

`sudo aptitude -y install nginx`

Remove the default settings for Nginx

`cd /etc/nginx/sites-available`

`sudo rm default ../sites-enabled/default`

Create the new configuration file

`sudo touch jenkins`

Copy the following code into the newly created `jenkins` file.

    upstream app_server {
      server 127.0.0.1:8080 fail_timeout=0;
    }

    server {
      listen 80;
      listen [::]:80 default ipv6only=on;
      server_name ;

      location / {
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Host $http_host;
        proxy_redirect off;

        if (!-f $request_filename) {
            proxy_pass http://app_server;
            break;
        }
      }
    }

Create a symbolic link between sites-available and sites-enabled:

`sudo ln -s /etc/nginx/sites-available/jenkins /etc/nginx/sites-enabled/`

Restart the Nginx proxy service

`sudo service nginx restart`

Jenkins will now be accessible from port 80.



## Jenkins full Introduction in one place
**1. Jenkins :**

Jenkins is an open source continuous integration tool written in Java. The project was forked from Hudson after a dispute with Oracle.

In a nutshell, Jenkins is the leading open source automation server. Built with Java, it provides hundreds of plugins to support building, testing, deploying and automation for virtually any project.

**Features :** 
Jenkins offers the following major features out of the box, and many more can be added through plugins: 

Easy installation: Just run java -jar jenkins.war, deploy it in a servlet container. No additional install, no database. Prefer an installer or native package? We have those as well. 
Easy configuration: Jenkins can be configured entirely from its friendly web GUI with extensive on-the-fly error checks and inline help. 
Rich plugin ecosystem: Jenkins integrates with virtually every SCM or build tool that exists. View plugins. 
Extensibility: Most parts of Jenkins can be extended and modified, and it's easy to create new Jenkins plugins. This allows you to customize Jenkins to your needs. 
Distributed builds: Jenkins can distribute build/test loads to multiple computers with different operating systems. Building software for OS X, Linux, and Windows? No problem. 

**Installation :**

    $ wget -q -O - https://jenkins-ci.org/debian/jenkins-ci.org.key | sudo apt-key add -
    
    $ sudo sh -c 'echo deb http://pkg.jenkins-ci.org/debian binary/ > /etc/apt/sources.list.d/jenkins.list'
    $ sudo apt-get update
    $ sudo apt-get install jenkins
    to do more refer link :

Ref : https://wiki.jenkins-ci.org/display/JENKINS/Installing+Jenkins+on+Ubuntu

Ref : http://www.vogella.com/tutorials/Jenkins/article.html

Ref : https://wiki.jenkins-ci.org/display/JENKINS/Meet+Jenkins

**JENKINS_HOME** directory
Jenkins needs some disk space to perform builds and keep archives. You can check this location from the configuration screen of Jenkins.
By default, this is set to ~/.jenkins, but you can change this in one of the following ways:
Set "JENKINS_HOME" environment variable to the new home directory before launching the servlet container.
Set "JENKINS_HOME" system property to the servlet container.
Set JNDI environment entry "JENKINS_HOME" to the new directory.
See the container specific documentation collection for more about how to do this for your container.
You can change this location after you've used Jenkins for a while, too. To do this, stop Jenkins completely, move the contents from old JENKINS_HOME to the new home, set the new JENKINS_HOME, and restart Jenkins.
JENKINS_HOME has a fairly obvious directory structure that looks like the following:

**JENKINS_HOME**

 

    +- config.xml     (jenkins root configuration)
     +- *.xml          (other site-wide configuration files)
     +- userContent    (files in this directory will be served under your http://server/userContent/)
     +- fingerprints   (stores fingerprint records)
     +- plugins        (stores plugins)
     +- workspace (working directory for the version control system)
         +- [JOBNAME] (sub directory for each job)
     +- jobs
         +- [JOBNAME]      (sub directory for each job)
             +- config.xml     (job configuration file)
             +- latest         (symbolic link to the last successful build)
             +- builds
                 +- [BUILD_ID]     (for each build)
                     +- build.xml      (build result summary)
                     +- log            (log file)
                     +- changelog.xml  (change log)

**Jenkins Build Jobs :** 

Creating a new build job in Jenkins is simple: just click on the “New Job” menu item on the Jenkins dashboard. Jenkins supports several different types of build jobs, which are presented to you when you choose to create a new job

**Freestyle software project**

Freestyle build jobs are general-purpose build jobs, which provides a maximum of flexibility.

**Maven project**
The “maven2/3 project” is a build job specially adapted to Maven projects. Jenkins understands Maven pom files and project structures, and can use the information gleaned from the pom file to reduce the work you need to do to set up your project.

**Workflow**

Orchestrates long-running activities that can span multiple build slaves. Suitable for building pipelines and/or organizing complex activities that do not easily fit in free-style job type.

Monitor an external job
The “Monitor an external job” build job lets you keep an eye on non-interactive processes, such as cron jobs.

Multiconfiguration job
The “multiconfiguration project” (also referred to as a “matrix project”) lets you run the same build job in many different configurations. This powerful feature can be useful for testing an application in many different environments, with different databases, or even on different build machines. 

**1. Building a software project (free style)**

Jenkins can be used to perform the typical build server work, such as doing continuous/official/nightly builds, run tests, or perform some repetitive batch tasks. This is called "free-style software project" in Jenkins.
Setting up the project
Go to Jenkins top page, select "New Job", then choose "Build a free-style software project". This job type consists of the following elements:
optional SCM, such as CVS or Subversion where your source code resides.
optional triggers to control when Jenkins will perform builds.
some sort of build script that performs the build (ant, maven, shell script, batch file, etc.) where the real work happens
optional steps to collect information out of the build, such as archiving the artifacts and/or recording javadoc and test results.
optional steps to notify other people/systems with the build result, such as sending e-mails, IMs, updating issue tracker, etc.

**Builds for Non-Source Control Projects**
There is sometimes a need to build a project simply for demonstration purposes or access to a SVN/CVS repository is unavailable. By choosing to configure the project  as "None" under "Source Code Management" you will have to:

1. Build the Project at least once, (it will fail), but Jenkins will create the structure jenkins/workspace/PROJECTNAME/
2. Copy the project files to jenkins/workspace/PROJECTNAME/
3. Build again and configure appropriately

**Jenkins Set Environment Variables**

 When a Jenkins job executes, it sets some environment variables that you may use in your shell script, batch command, Ant script or Maven POM .  See the list of variable by clicking on ENVIRONMENT_VARIABLE

**Configuring automatic builds**

Builds in Jenkins can be triggered periodically (on a schedule, specified in configuration), or when source changes in the project have been detected, or they can be automatically triggered by requesting the URL:

http://YOURHOST/jenkins/job/PROJECTNAME/build

This allows you to hook Jenkins builds into a variety of setups. For more information (in particular doing this with security enabled), see Remote access API.

**Builds by source changes**

You can have Jenkins poll your Revision Control System for changes. You can specify how often Jenkins polls your revision control system using the same syntax as crontab on Unix/Linux. However, if your polling period is shorter than it takes to poll your revision control system, you may end up with multiple builds for each change. You should either adjust your polling period to be longer than the amount of time it takes to poll your revision control system, or use a post-commit trigger. You can examine the Polling Log for each build to see how long it took to poll your system.

Alternatively, instead of polling on a fixed interval, you can use a URL trigger (described above), but with /polling instead of /build at the end of the URL. This makes Jenkins poll the SCM for changes rather than building immediately. This prevents Jenkins from running a build with no relevant changes for commits affecting modules or branches that are unrelated to the job. When using /polling the job must be configured for polling, but the schedule can be empty.

**Builds by e-mail (sendmail)**

If you have the root account of your system and you are using sendmail, I found it the easiest to tweak /etc/aliases and add the following entry:
jenkins-foo: "|/bin/wget -o /dev/null 

http://YOURHOST/jenkins/job/PROJECTNAME/build"

and then run "newaliases" command to let sendmail know of the change. Whenever someone sends an e-mail to "jenkins-foo@yoursystem", this will trigger a new build. See this for more details about configuring sendmail.
Builds by e-mail (qmail)
With qmail, you can write /var/qmail/alias/.qmail-jenkins as follows:
|/bin/wget -o /dev/null http://YOURHOST/jenkins/job/PROJECTNAME/build"

**2. Building a Maven project**


Jenkins provides a job type dedicated to Maven 2/3. This job type integrates Jenkins deeply with Maven 2/3 and provides the following benefits compared to the more generic free-style software project.

Jenkins parses Maven POMs to obtain much of the information needed to do its work. As a result, the amount of configuration is drastically reduced.

Jenkins listens to Maven execution and figures out what should be done when on its own. For example, it will automatically record the JUnit report when Maven runs the test phase. Or if you run the javadoc goal, Jenkins will automatically record javadoc.

Jenkins automatically creates project dependencies between projects which declare SNAPSHOT dependencies between each other. See below.
Thus mostly you just need to configure SCM information and what goals you'd like to run, and Jenkins will figure out everything else.


**This project type can automatically provide the following features:**

Archive artifacts produced by a build

Publish test results

Trigger jobs for projects which are downstream dependencies

Deploy your artifacts to a Maven repository

Breakout test results by module

Optionally rebuild only changed modules, speeding your builds

Automatic build chaining from module dependencies

Jenkins reads dependencies of your project from your POM, and if they are also built on Jenkins, triggers are set up in such a way that a new build in one of those dependencies will automatically start a new build of your project. Jenkins understands all kinds of dependencies in POM.
 Namely,parent POM

    <dependencies> section of your project
    <plugins> section of your project
    <extensions> section of your project
    <reporting> section of your project

This process takes versions into account, so you can have multiple versions/branches of your project on the same Jenkins and it will correctly determine dependencies.
 **Note** that dependency version ranges are not supported, see [https://issues.jenkins-ci.org/browse/JENKINS-2787][1] for the reason.

This feature can be disabled on demand - see configuration option Build whenever a SNAPSHOT dependency is built



**Installation :**

1 . go into Manage Jenkins>>configure System
2. in maven tab “Click on maven installation......

You can either get Jenkins to install a specific version of Maven automatically , or provide a path to a local Maven installation (You can configure as many versions of Maven for your build projects as you want, and use different versions of Maven for different projects.
If you tick the Install automatically checkbox, Jenkins will download and install the requested version of Maven for you and install it to the tools directory in the Jenkins home directory.


**How to Use It**

First, you must configure a Maven installation (this step can be skipped if you are using DEV@cloud). This can be done by going to the system configuration screen (Manage Jenkins-> Configure System). In the “Maven Installations” section, 1) click the Add button, 2) give it a name such as “Maven 3.0.3” and then 3) choose the version from the drop down.


Now, Jenkins will automatically install this version any time it’s needed (on any new build machines, for example) by downloading it from Apache and unzipping it.

**Create a new Maven Job :**

1. Clicking “New Job / New Item” on left hand
2. Give it a name
3. Choose the “Build a Maven 2/3 project”
4. Save your job

Now you need to configure of your job

1.  Choose the SCM you want to use (ex. Using git)
2. choose maven target to call
3. add Repository URL and Credential.

4. check user private maven repo:

  You can also define the custome path for the same.

5 . Build Project 

Build your project by clicking on build now and click on the progress bar in the left hand “Build Executor Status” to watch jenkins install Maven, checkout your project, and build it using maven.


**Logging:** 

https://wiki.jenkins-ci.org/display/JENKINS/Logging

**Script Console :**

Useful for trouble-shooting, diagnostics or batch updates of jobs Jenkins provides a script console which gives you access to all Jenkins internals.
These scripts are written in Groovy and you'll find some samples of them in this [page][1].


  [1]: https://wiki.jenkins-ci.org/display/JENKINS/Logging

## Move Jenkins from one PC to another
This worked for me to move from Ubuntu 12.04 (Jenkins ver. 1.628) to Ubuntu 16.04 (Jenkins ver. 1.651.2). I first [installed Jenkins from the repositories][1]. 

 1. [Stop both Jenkins servers][2]
 2. Copy `JENKINS_HOME` (e.g. /var/lib/jenkins) from the old server to the new one. From a console in the new server:

    `rsync -av username@old-server-IP:/var/lib/jenkins/ /var/lib/jenkins/`

 3. [Start your new Jenkins server][2]

You might not need this, but I had to

 - `Manage Jenkins` and `Reload Configuration from Disk`.
 - Disconnect and connect all the slaves again.
 - Check that in the `Configure System > Jenkins Location`, the `Jenkins URL` is correctly assigned to the new Jenkins server.


  [1]: https://wiki.jenkins-ci.org/display/JENKINS/Installing+Jenkins+on+Ubuntu
  [2]: https://cloudbees.zendesk.com/hc/en-us/articles/216118748-How-to-Start-Stop-or-Restart-your-Instance

## Installation
> For apt-get based systems such as Ubuntu

Add the Jenkins repository:

`wget -q -O - https://jenkins-ci.org/debian/ Jenkins-ci.org.key | sudo apt-key `

    sudo sh -c 'echo deb http://pkg.jenkins-ci.org/debian-stable binary/ > /etc/apt/sources.list.d/jenkins.list'

Update sources and install Jenkins:

`sudo apt-get update`

`sudo apt-get install jenkins`

A jenkins user is now created and by default Jenkins will be running on port 8080.

> For RPM Based distributions such as Red Hat Enterprise Linux (RHEL), CentOS, Fedora or Scientific Linux

To download the repository file for the stable version:

`sudo wget -O /etc/yum.repos.d/jenkins.repo http://pkg.jenkins-ci.org/redhat-stable/jenkins.repo`

Or if you want the latest weekly releases:

`sudo wget -O /etc/yum.repos.d/jenkins.repo http://pkg.jenkins-ci.org/redhat/jenkins.repo`

Import the public key:

`sudo rpm --import https://jenkins-ci.org/redhat/jenkins-ci.org.key`

Install Jenkins using yum:

`sudo yum install jenkins`

Jenkins requires java in order to run, to install it:

`sudo yum install java`

To start/stop/restart jenkins use:

`sudo service jenkins start/stop/restart`

## Upgrading jenkins(RPM installations)

 1. Backup jenkins home directory
 2. Replace jenkins.war in following location with new WAR file.
/usr/lib/jenkins/jenkins.war`
 3. Restart Jenkins
 4. Check pinned plugins and unpin if required
 5. Reload Configuration from Disk

*note: For Jenkins 2 upgrades for bundled jetty app server,disable AJP port(set `JENKINS_AJP_PORT="-1"`) in  `/etc/sysconfig/jenkins`.* 



## Configure a project in Jenkins
Here we will be checking out the latest copy of our project's code, run the tests and will make the application live.To achieve that, follow below steps:

1. Open Jenkins in browser.
2. Click the ***New Job*** link.
3. Enter project name and select the ***Build a free-style software project*** link.
4. Click on ***Ok*** button.
5. Under the ***Source code management section***, select the radio box next to your source code management tool. In my case I have selected ***Git***.

Provide url of git repo like `git://github.com/example/example.git`
6. Under the ***Build triggers***, select the radio box next to ***Poll SCM***.
7. Provide `*****` in ***Schedule*** box. This box is responsible to trigger the build at regular intervals. `*****` specifies that, the job will get trigger every minute for changes in git repo.
8. Under the ***Build*** section, click the ***Add Build Step*** button and then select the option by which you want to build the project. I have selected ***Execute Shell***. In the command box write the command to build,run the tests, and deploy it to prod.
10. Scroll down and ***Save***.

So above we have configured a basic project in Jenkins which will trigger the build at every minute for change in your git repository.
Note: To setup the complex project, you may have to install some plugins in Jenkins.


## Configure a simple build project with Jenkins 2 pipeline script
Here we will be creating a Groovy pipeline in Jenkins 2 to do the following steps :

- Verify every 5 minutes if new code has been commited to our project
- Checkout code from SCM repo
- Maven compile of our Java code
- Run our integration tests and publish the results

Here are the steps we will :

 1. Make sure we have at least a 2.0 Jenkins version (you can check that in the bottom-right corner of your page) such as :

      [![Jenkins version capture example][1]][1]

1. On Jenkins home page, click on **New Item**
1. Enter project name and select **Pipeline**
1. In **Build Triggers** section, select **Poll SCM** option and add the following 5 minutes CRON schedule :
`*/5 * * * *`
1. In **Pipeline** section, choose either **Pipeline Script** or **Pipeline Script from SCM**
1. If you selected **Pipeline Script from SCM** on previous step, you now need to specify your SCM repository (Git, Mercurial, Subversion) URL in **Repository URL** such as `http://github.com/example/example.git`. You also need to specify the **Script Path** of your Groovy script file in your example.git repository, e.g. `pipelines/example.groovy`
1. Copy the following Groovy code, either directly in the Groovy script window if you previously clicked **Pipeline Script** or in your `example.groovy` if you choosed **Pipeline Script from SCM**


    node('remote') {
        // Note : this step is only needed if you're using direct Groovy scripting
        stage 'Checkout Git project'
        git url: 'https://github.com/jglick/simple-maven-project-with-tests.git'
        def appVersion = version()
        if (appVersion) {
            echo "Building version ${appVersion}"
        }
    
        stage 'Build Maven project'
        def mvnHome = tool 'M3'
        sh "${mvnHome}/bin/mvn -B -Dmaven.test.failure.ignore verify"
        step([$class: 'JUnitResultArchiver', testResults: '**/target/surefire-reports/TEST-*.xml'])
    }
    def version() {
        def matcher = readFile('pom.xml') =~ '<version>(.+)</version>'
        matcher ? matcher[0][1] : null
    }

Here you go, you should now be able to compile and test your first Jenkins project using Jenkins 2 Groovy pipeline.

  [1]: http://i.stack.imgur.com/NCqh1.png



## Installing Plugin from external source
    java -jar [Path to client JAR] -s [Server address] install-plugin [Plugin ID]

The client JAR must be the CLI JAR file, not the same JAR/WAR that runs Jenkins itself. Unique IDs can be found on a plugins respective page on the Jenkins CLI wiki (https://wiki.jenkins-ci.org/display/JENKINS/Plugins)

