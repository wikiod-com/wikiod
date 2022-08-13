---
title: "POM - Project Object Model"
slug: "pom---project-object-model"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## POM structure
Project Object Model is the basic unit of Maven and defines the project structure, dependencies, etc.

The following are very minimal to create a POM:

- `project` root
- `modelVersion` – should be set to `4.0.0`
- `groupId` – the ID of the project's group
- `artifactId` – the ID of the artifact (project)
- `version` – the version of the artifact under the specified group

`groupId`, `artifactId` and `version` are called *Maven coordinates* and sometimes abbreviated with *GAV*. They uniquely identify the resulting artifact of a project in a Maven repository (and _should_ do so in the entire universe).

A minimal sample POM looks like:

    <project>
      <modelVersion>4.0.0</modelVersion>

      <groupId>com.sample</groupId>
      <artifactId>sample-app</artifactId>
      <version>0.0.1-SNAPSHOT</version>
    </project>

## POM Aggregation
The modules of a multi-module project are aggregated from a hierarchical structure.

The root `pom` packing should look like:

    <packaging>pom</packaging>

The following will be the directory structure of the project:

     |-- sample-app
      \   `-- pom.xml
       |-- sample-module-1
       |   `-- pom.xml
       |-- sample-module-2
       |   `-- pom.xml

Root POM:

    <project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.sample</groupId>
      <artifactId>sample-app</artifactId>
      <version>0.0.1-SNAPSHOT</version>
      <packaging>pom</packaging>
     
      <modules>
        <module>sample-module-1</module>
        <module>sample-module-2</module>
      </modules>
      <dependencyManagement>
        ...
      </dependencyManagement>
    </project>

## POM Inheritance
Inheritance is the biggest asset of the POM, where the following can be managed from super POM to child POM.

- dependencies
- developers and contributors
- plugin lists (including reports)
- plugin executions with matching ids
- plugin configuration

The following enables the inheritance

      <parent>
        <groupId>com.sample</groupId>
        <artifactId>sample-app-parent</artifactId>
        <version>1.0.0</version>
      </parent>

POM structure looks like

    <project>
      <parent>
        <groupId>com.sample</groupId>
        <artifactId>sample-app-parent</artifactId>
        <version>1.0.0</version>
      </parent>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.sample</groupId>
      <artifactId>sample-app</artifactId>
      <version>0.0.1-SNAPSHOT</version>
    </project>

