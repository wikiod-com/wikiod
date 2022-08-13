---
title: "Maven Assembly Plugin"
slug: "maven-assembly-plugin"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Creating .jar file with all the dependencies of the project
To create a JAR containing all of its dependencies, it is possible to use the built-in descriptor format [`jar-with-dependencies`](http://maven.apache.org/plugins/maven-assembly-plugin/descriptor-refs.html#jar-with-dependencies). The following example configures an execution of the Assembly Plugin bound to the `package` phase, using this built-in descriptor and declaring a main class of `com.example`:

    <plugin>
      <artifactId>maven-assembly-plugin</artifactId>
      <version>2.6</version>
      <executions>
        <execution>
          <id>make-assembly</id>
          <phase>package</phase>
          <goals>
            <goal>single</goal>
          </goals>
          <configuration>
            <archive>
              <manifest>
                  <mainClass>com.example</mainClass>
              </manifest>
            </archive>
            <descriptorRefs>
              <descriptorRef>jar-with-dependencies</descriptorRef>
            </descriptorRefs>
          </configuration>
        </execution>
      </executions>
    </plugin>

Running:

    mvn clean package

on the command-line will result in the jar-with-dependencies to be built and attached to the project.

If more control over this [uber-jar](http://stackoverflow.com/questions/11947037/what-is-an-uber-jar) is needed, turn to the [Maven Shade Plugin](https://maven.apache.org/plugins/maven-shade-plugin/).

