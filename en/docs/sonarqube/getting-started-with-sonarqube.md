---
title: "Getting started with sonarqube"
slug: "getting-started-with-sonarqube"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Sonarqube uses database for storing its results and analysis. You can install MySQL for example and run it using `mysql -u root -p` and then run the following queries to set up the database tables.

    CREATE DATABASE sonar CHARACTER SET utf8 COLLATE utf8_general_ci;
    CREATE USER 'sonar' IDENTIFIED BY 'sonar';
    GRANT ALL ON sonar.* TO 'sonar'@'%' IDENTIFIED BY 'sonar';
    GRANT ALL ON sonar.* TO 'sonar'@'localhost' IDENTIFIED BY 'sonar';
    FLUSH PRIVILEGES;

then you need to download the sonarqube from their website, for example you can use `wget` to do it as shown below. Choose the appropriate sonarqube version required.

    wget https://sonarsource.bintray.com/Distribution/sonarqube/sonarqube-5.6.zip
    
    unzip sonarqube-5.6.zip
    mv sonarqube-5.6 /opt/sonar

Open /opt/sonar/conf/sonar.properties with vim editor, and modify it as shown below.

    sonar.jdbc.username=sonar
    sonar.jdbc.password=sonar
    
    sonar.jdbc.url=jdbc:mysql://localhost:3306/sonar?useUnicode=true&characterEncoding=utf8&rewriteBatchedStatements=true&useConfigs=maxPerformance

these setup the database and the user name, password for that. To setup server on port 9000 add the below configurations

    sonar.web.host=127.0.0.1
    sonar.web.context=/sonar
    sonar.web.port=9000

this sets up all the required configurations. now you can start the service using this command `sudo /opt/sonar/bin/linux-x86-64/sonar.sh start`. replace the start keyword with `stop` to shutdown the server. 

for more information and configurations you can visit - http://docs.sonarqube.org/display/SONAR/Installing+the+Server

