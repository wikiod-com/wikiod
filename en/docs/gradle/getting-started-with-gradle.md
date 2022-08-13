---
title: "Getting started with gradle"
slug: "getting-started-with-gradle"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Gradle Installation
Requirements: Installed Java JDK or JRE (version 7 or higher for Gradle 3.x version)

Installation steps:

 1. Download Gradle distribution from the [official web site][1]
 2. Unpack the ZIP
 3. Add the `GRADLE_HOME` environment variable. This variable should point to the unpacked files from the previous step.
 4. Add `GRADLE_HOME/bin` to your `PATH` environment variable, so you can run Gradle from the command line interface (CLI)
 5. Test your Gradle installation by typing `gradle -v` in the CLI. The output should contain the installed Gradle version and the current Gradle configuration details

More information can be found in the [official user guide][2]


  [1]: https://gradle.org/gradle-download/
  [2]: https://docs.gradle.org/current/userguide/installation.html

## Installation with homebrew on OS X / macOS
Users of [homebrew][1] can install gradle by running

    brew install gradle


  [1]: http://brew.sh

## Installing with SdkMan
Users of [SdkMan][1] can install Gradle by running:

    sdk install gradle

Install specific version

    sdk list gradle
    sdk install gradle 2.14

Switch versions

    sdk use gradle 2.12

  [1]: http://sdkman.io

## Install Gradle plugin for Eclipse
Here are the steps required to install Gradle plugin in Eclipse: 
 1. Open Eclipse and go to **Help** -> **Eclipse Marketplace**
 2. In the search bar, enter **buildship** and hit enter
 3. Select **"Buildship Gradle Integration 1.0"** and click **Install**
 4. In the next window, click **Confirm**
 5. Then, **accept** the terms and license of agreement, then click **Finish**
 6. After installation, Eclipse will need to restart, click **Yes**



## Hello World
Gradle tasks can be written using Groovy code from inside a projects build.gradle file. These tasks can then be executed using `> gradle [taskname]` at the terminal or by executing the task from within an IDE such as Eclipse. 

To create the Hello World example in gradle we must define a task that will print a string to the console using Groovy. We will use Groovy's `printLn` to call Java's `System.out.printLn` method to print the text to the console. 

**build.gradle**

    task hello {
        doLast {
            println 'Hello world!'
        }
    }
We can then execute this task by using `> gradle hello` or `> gradle -q hello`. The `-q` is used to suppress gradle log messages so that only the output of the task will be shown. 

**Output of `> gradle -q hello`:**

    > gradle -q hello
    Hello world!

## More about tasks
First of all: operator `<<` (leftShift) is equivalent of `doLast {closure}`. From **gradle 3.2** it is **deprecated**. All the task code are writing in a **build.gradle**.

> A task represents some atomic piece of work which a build performs.
> This might be compiling some classes, creating a JAR, generating
> Javadoc, or publishing some archives to a repository.

Gradle supports two big types of tasks: simple and enhanced.

Let's observe some task definition styles:

    task hello {
        doLast{
           //some code
        }    
    }

Or the:

    task(hello) {
        doLast{
           //some code
        }    
    }

This tasks above are equivalents. Also, you can provide some extensions to the task, such as: `dependsOn`,`mustRunAfter`, `type` etc. 
You can extend task by adding actions after task definition, like this:

    task hello {
        doLast{
           println 'Inside task'
        }    
    }
    hello.doLast {
        println 'added code'
    }
When we'll execute this we got:

    > gradle -q hello
        Inside task
        added code

Questions about task dependencies and ordering examined [here](https://www.wikiod.com/gradle/task-dependencies)
----------
Let's talk about two big types of task.

Simple:
========
Tasks which we define with an action closure:

        task hello {
            doLast{
            println "Hello from a simple task"
            }
        }

Enhanced
========
Enhanced it is a task with a preconfigured behavior. All plugins that you using in your project are the *extended*, or the **enhanced tasks**. Let's create ours and you will understand how it works:

    task hello(type: HelloTask)
    
    class HelloTask extends DefaultTask {
        @TaskAction
        def greet() {
            println 'hello from our custom task'
        }
    }
Also, we can pass parameters to our task, like this:

    class HelloTask extends DefaultTask {
        String greeting = "This is default greeting"
        @TaskAction
        def greet() {
            println greeting
        }
    }

And from now on we can rewrite our task like so:
    
       //this is our old task definition style
    task oldHello(type: HelloTask) 
       //this is our new task definition style     
    task newHello(type: HelloTask) {
        greeting = 'This is not default greeting!'
    }
When we'll execute this we got:
    
    > gradle -q oldHello
    This is default greeting

    > gradle -q newHello
    This is not default greeting!
    
All questions about development gradle plugins onto [official site](https://docs.gradle.org/current/userguide/custom_plugins.html)

