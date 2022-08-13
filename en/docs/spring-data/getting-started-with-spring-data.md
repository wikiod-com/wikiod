---
title: "Getting started with spring-data"
slug: "getting-started-with-spring-data"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Spring Data is a project consisting of a number of subprojects. The most common ones are [Spring Data JPA](http://stackoverflow.com/questions/tagged/spring-data-jpa), [Spring Data MongoDB](http://stackoverflow.com/questions/tagged/spring-data-mongodb), [Spring Data Elasticsearch][1], [Spring Data Neo4J](http://stackoverflow.com/questions/tagged/spring-data-neo4j), [Spring Data Cassandra](http://stackoverflow.com/questions/tagged/spring-data-cassandra) and [Spring Data Redis](http://stackoverflow.com/questions/tagged/spring-data-cassandra).

Unless you are developing your own subproject based upon Spring Data, it is highly unlikely that you will need to use it directly in your application. See the individual subprojects for details on their installation and setup. If however, you do have the need to use Spring Data in your application directly, the following instructions will be helpful.

> **Using Maven**

    <dependencies>
      <dependency>
        <groupId>org.springframework.data</groupId>
        <artifactId>spring-data-commons</artifactId>
        <version>[version-number]</version>
      </dependency>
    </dependencies>

> **Using Gradle**

    dependencies {
      compile 'org.springframework.data:spring-data-commons:[version-number]'
    }

Substitute *[version number]* with the Spring Data version you wish to use.


  [1]: https://stackoverflow.com/questions/tagged/spring-data-elasticsearch

