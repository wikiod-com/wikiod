---
title: "Installing the Spring Boot CLI"
slug: "installing-the-spring-boot-cli"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

The [Spring Boot CLI](http://docs.spring.io/spring-boot/docs/current/reference/html/getting-started-installing-spring-boot.html#getting-started-installing-the-cli) allows you to easily create and work with Spring Boot applications from the command-line.

Once installed, the Spring Boot CLI can be run using the `spring` command:

To get command-line help:

    $ spring help

To create and run your first Spring Boot Project:

    $ spring init my-app
    $ cd my-app
    $ spring run my-app

Open your browser to `localhost:8080`:

    $ open http://localhost:8080

You'll get the whitelabel error page because you haven't yet added any resources to your application, but you're all ready to go with just the following files:

    my-app/
    ├── mvnw
    ├── mvnw.cmd
    ├── pom.xml
    └── src/
        ├── main/
        │   ├── java/
        │   │   └── com/
        │   │       └── example/
        │   │           └── DemoApplication.java
        │   └── resources/
        │       └── application.properties
        └── test/
            └── java/
                └── com/
                    └── example/
                        └── DemoApplicationTests.java

- `mvnw` and `mvnw.cmd` - Maven wrapper scripts that will download and install Maven (if necessary) on the first use.
- `pom.xml` - The Maven project definition
- `DemoApplication.java` - the main class that launches your Spring Boot application.
- `application.properties` - A file for externalized configuration properties. (Can also be given a `.yml` extension.)
- `DemoApplicationTests.java` - A unit test that validates initialization of the Spring Boot application context.


## Install on Mac OSX with HomeBrew
    $ brew tap pivotal/tap
    $ brew install springboot

## Manual Installation

See the [download page](http://docs.spring.io/spring-boot/docs/current/reference/html/getting-started-installing-spring-boot.html#getting-started-manual-cli-installatio) to manually download and unpack the latest version, or follow the links below:

* [spring-boot-cli-1.5.1.RELEASE-bin.zip](http://repo.spring.io/release/org/springframework/boot/spring-boot-cli/1.5.1.RELEASE/spring-boot-cli-1.5.1.RELEASE-bin.zip)

* [spring-boot-cli-1.5.1.RELEASE-bin.tar.gz](http://repo.spring.io/release/org/springframework/boot/spring-boot-cli/1.5.1.RELEASE/spring-boot-cli-1.5.1.RELEASE-bin.tar.gz)

## Install on Mac OSX with MacPorts
    $ sudo port install spring-boot-cli

## Install on any OS with SDKMAN!
[SDKMAN!](http://sdkman.io/) is the Software Development Kit Manager for Java. It can be used to install and manage versions of the Spring Boot CLI as well as Java, Maven, Gradle, and more.

    $ sdk install springboot

