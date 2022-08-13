---
title: "Jenkins CI setup for Android Projects"
slug: "jenkins-ci-setup-for-android-projects"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Step by step approach to set up Jenkins for Android
This is a step by step guide to set up the automated build process using Jenkins CI for your Android projects. The following steps assume that you have new hardware with just any flavor of Linux installed. It is also taken into account that you might have a remote machine.

# PART I: Initial setup on your machine

1) Log in via _ssh_ to your Ubuntu machine:
    >ssh username@xxx.xxx.xxx

2) Download a version of the Android SDK on your machine:
    > wget https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz

3) Unzip the downloaded _tar_ file:
    > sudo apt-get install tar  
    > tar -xvf android-sdk_r24.4.1-linux.tgz

4) Now you need to install Java 8 on your Ubuntu machine, which is a requirement for Android builds on Nougat. Jenkins would require you to install JDK and JRE 7 using the steps below:
    > sudo apt-get install python-software-properties  
    > sudo add-apt-repository ppa:webupd8team/java  
    > sudo apt-get update  
    > apt-get install openjdk-8-jdk  

5) Now install Jenkins on your Ubuntu machine:
    > wget -q -O - https://pkg.jenkins.io/debian/jenkins-ci.org.key | sudo apt-key add -  
    > sudo sh -c 'echo deb http://pkg.jenkins.io/debian-stable binary/ > /etc/apt/sources.list.d/jenkins.list'  
    > sudo apt-get update  
    > sudo apt-get install jenkins

6) Download the latest supported Gradle version for your Android setup:
    > wget https://services.gradle.org/distributions/gradle-2.14.1-all.zip  
    > unzip gradle-2.14.1-all.zip

7) Set up Android on your Ubuntu machine. First move to the _tools_ folder in the Android SDK folder downloaded in step 2:

    > cd android-sdk-linux/tools **// lists available SDK**  
    > android update sdk --no-ui **// Updates SDK version**  
    > android list sdk -a | grep "SDK Build-tools" **// lists available build tools**  
    > android update sdk -a -u -t 4   **// updates build tools version to one listed as 4 by prev. cmd.**  
    > update java

8) Install _Git_ or any other VCS on your machine:
    > sudo apt-get install git

9) Now log in to Jenkins using your internet browser. Type **`ipAddress:8080`** into the address bar.

10) In order to receive the password for the first-time login, please check the corresponding file as follows (you will need su permissions to access this file):
    > cat /var/lib/jenkins/secrets/initialAdminPassword

# PART II: Set up Jenkins to build Android Jobs

1) Once logged in, go to the following path:
    > Jenkins > Manage Jenkins > Global Tool Configuration

2) At this location, add `JAVA_HOME` with the following entries:
    > Name = JAVA_HOME  
    > JAVA_HOME = /usr/lib/jvm/java-8-openjdk-amd64

3) Also add the following values to _Git_ and save the environment variables:
    > Name = Default  
    > /usr/bin/git

4) Now go to the following path:
    > Jenkins > Manage Jenkins > Configuration

5) At this location, add `ANDROID_HOME` to the "global properties":
    > Name = ANDROID_HOME  
    > Value = /home/username/android-sdk-linux

# Part III: Create a Jenkins Job for your Android project

1) Click on _New Item_ in the Jenkins home screen.

2) Add a _Project Name_ and _Description_.

3) In the _General_ tab, select _Advanced_. Then select _Use custom workspace_:
    > Directory /home/user/Code/ProjectFolder

4) In the source code management select _Git_. I am using _Bitbucket_ for the purpose of this example:
    > Repository URL = https://username:password@bitbucket.org/project/projectname.git

5) Select additional behaviors for your repository:
    > Clean Before Checkout  
    > Checkout to a sub-directory. Local subdirectory for repo /home/user/Code/ProjectFolder

6) Select a branch you want to build:
    > */master

7) In the _Build_ tab, select _Execute Shell_ in _Add build step_.

8) In the _Execute shell_, add the following command:
    > cd /home/user/Code/ProjectFolder && gradle clean assemble --no-daemon

9) If you want to run Lint on the project, then add another build step into the _Execute shell_:
    > /home/user/gradle/gradle-2.14.1/bin/gradle lint

Now your system is finally set up to build Android projects using Jenkins. This setup makes your life so much easier for releasing builds to QA and UAT teams.

PS: Since Jenkins is a different user on your Ubuntu machine, you should give it rights to create folders in your workspace by executing the following command:
> chown -R jenkins .git

