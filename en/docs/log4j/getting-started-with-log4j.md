---
title: "Getting started with log4j"
slug: "getting-started-with-log4j"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
## Installation ##
Installation of Log4j2 is as simple as putting log4j2 jar in application classpath. Though you might want to [customize logs output through additional config file][1]
## Configuration ##
### maven ###
To add log4j to project in maven, add it's dependency:
In pom.xml add following dependency:

    <dependencies>
      <dependency>
        <groupId>org.apache.logging.log4j</groupId>
        <artifactId>log4j-api</artifactId>
        <version>${log4j2.version}</version>
      </dependency>
      <dependency>
        <groupId>org.apache.logging.log4j</groupId>
        <artifactId>log4j-core</artifactId>
        <version>${log4j2.version}</version>
      </dependency>
    </dependencies>


### springboot with maven ###

Spring-boot is commonly used framework for web application. It features support auto-configuration for many features including logging façade like log4j2.
To add log4j2 to your spring-boot project make sure you exclude default logging façade: commons-logging.
Log4j will be used, when it is only logging façade on classpath.

    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
            <!-- exclude spring-boot java commons logging in favour of log4j2 -->
            <exclusions>
                <exclusion>
                    <groupId>org.springframework.boot</groupId>
                    <artifactId>spring-boot-starter-logging</artifactId>
                </exclusion>
            </exclusions>
        </dependency>
        <!-- add log4j2 to spring-boot: -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-log4j2</artifactId>
        </dependency>

Notice, that there is no version in above snippet. It is because project inherit version from parent. Make sure you also inherit from spring-boot-starter-parent, by adding:

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>1.3.3.RELEASE</version>
    </parent>

### ivy ###
In ivy.xml, add following dependency:

    <dependencies>
      <dependency org="org.apache.logging.log4j" name="log4j-api" rev="${log4j2.version}" />
      <dependency org="org.apache.logging.log4j" name="log4j-core" rev="${log4j2.version}" />
    </dependencies>

### gradle ###
In your .gradle file:

    dependencies {
      compile group: 'org.apache.logging.log4j', name: 'log4j-api', version: '2.6.2'
      compile group: 'org.apache.logging.log4j', name: 'log4j-core', version: '2.6.2'
    }


  [1]: https://www.wikiod.com/log4j/log4j-logs-customization "Log4j logs output additional configuration file"

