---
title: "Getting started with spring-data-jpa"
slug: "getting-started-with-spring-data-jpa"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
To start using Spring data JPA, you must include the dependency in your project with the one of Spring core, all together. If you're using Maven as dependency management system (replace *version-number* for the version you want to use):

    <dependencies>
        <dependency>
            <groupId>org.springframework.data</groupId>
            <artifactId>spring-data-jpa</artifactId>
            <version>version-number</version>
        </dependency>
    </dependencies>

And if you're using Gradle:

    dependencies {
        compile 'org.springframework.data:spring-data-jpa:version-number'
    }

You can also set it up when using Spring Boot, just include the starter dependency and get rid of the version number:

    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
        </dependency>
    </dependencies>

## search by Entity property and search by Entity property in
    WarehosueEntity findWarehouseById(@Param("id") Long id);
    
    List<WarehouseEntity> findWarehouseByIdIn(@Param("idList") List<Long> warehouseIdList);

