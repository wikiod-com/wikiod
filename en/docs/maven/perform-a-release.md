---
title: "Perform a Release"
slug: "perform-a-release"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The standard Maven plugin used by a Release Process is the maven-release-plugin – the configuration for this plugin is minimal:

SCM in the Maven pom:The Release process will interact with the Source Control of the project – this means we need to define the "scm" element in our pom.xml.The "scm" element for a release build should contain enough information to check out the tag that was created for this release.

Note: Make sure to use maven release plugin 2.5 or later to avoid maven related issues.
The Release Process

    mvn release:clean

The above command will perform the below :
delete the release descriptor (release.properties)
delete any backup POM files

    mvn release:prepare

Next part of the Release process is Preparing the Release; this will:
perform some checks – there should be no uncommitted changes and the project should depend on no SNAPSHOT dependencies
change the version of the project in the pom file to a full release number (remove SNAPSHOT suffix) – in our example – 0.0.1
run the project test suites
commit and push the changes
create the tag out of this non-SNAPSHOT versioned code
increase the version of the project in the pom – in our example – 0.0.2-SNAPSHOT
commit and push the changes

    mvn release:perform

The latter part of the Release process is Performing the Release; this will:
checkout release tag from SCM
build and deploy released code
This second step of the process relies on the output of the Prepare step – the release.properties.


## POM.xml to perform release to Nexus repository
    <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <groupId>org.codezarvis.artifactory</groupId>
    <artifactId>nexusrelease</artifactId>
    <version>0.0.5-SNAPSHOT</version>
    <packaging>jar</packaging>
    
    <name>nexusrelease</name>
    <url>http://maven.apache.org</url>
    
    <scm>
    <connection>scm:git:git@github.com:isudarshan/nexuspractice.git</connection>
    <url>scm:git:git@github.com:isudarshan/nexuspractice.git</url>
    <developerConnection>scm:git:git@github.com:isudarshan/nexuspractice.git</developerConnection>
    <tag>HEAD</tag>
    </scm>
    
    <distributionManagement>
    <!-- Publish the versioned snapshot here -->
    <repository>
    <id>codezarvis</id>
    <name>codezarvis-nexus</name>
    <url>http://localhost:8080/nexus/content/repositories/releases</url>
    </repository>
    
    <!-- Publish the versioned releases here -->
    <snapshotRepository>
    <id>codezarvis</id>
    <name>codezarvis-nexus</name>
    <url>http://localhost:8080/nexus/content/repositories/snapshots</url>
    </snapshotRepository>
    </distributionManagement>
    
    <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
    
    <dependencies>
    <dependency>
    <groupId>junit</groupId>
    <artifactId>junit</artifactId>
    <version>3.8.1</version>
    <scope>test</scope>
    </dependency>
    </dependencies>
    
    <build>
    <pluginManagement>
    <plugins>
    <plugin>
    <artifactId>maven-release-plugin</artifactId>
    <version>2.5.2</version>
    <executions>
    <execution>
    <id>default</id>
    <goals>
    <goal>perform</goal>
    </goals>
    <configuration>
    <pomFileName>${project.name}/pom.xml</pomFileName>
    </configuration>
    </execution>
    </executions>
    </plugin>
    </plugins>
    </pluginManagement>
    </build>
    </project>

