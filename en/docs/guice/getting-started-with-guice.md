---
title: "Getting started with guice"
slug: "getting-started-with-guice"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setup of a 'Hello, world!' example
Guice is a Java library. To use it you have to add a [JAR file][1] into the classpath of your Java project.

# Sample classes

Below are several classes for a "Hello, world!" example.

An interface of a hello "service":

    public interface HelloWorldService {
        public void sayHello();
    }

The implementation of the service:

    public class HelloWorldServiceImpl implements HelloWorldService {
        @Override
        public void sayHello() {
            System.out.println("Hello, world!");
        }
    }

A Guice naming module. It is needed to instruct Guice that `HelloWorldServiceImpl` will be injected where a hello service is needed. 

    import com.google.inject.AbstractModule;
    
    public class HelloWorldModule extends AbstractModule {
        protected void configure() {
            bind(HelloWorldService.class).to(HelloWorldServiceImpl.class);
        }
    }

A main class where the actual injection of a hello service takes place:

    import javax.inject.Inject;
    
    import com.google.inject.Guice;
    import com.google.inject.Injector;
    import com.google.inject.Module;
    
    public class Main {
    
        @Inject
        private HelloWorldService service;//hello service
        
        public static void main(String[] args) {
            
            Main main = new Main();
            
            Module module = new HelloWorldModule();
            Injector injector = Guice.createInjector(module);
            injector.injectMembers(main);//injects the implementation of the service
            
            main.testGuice();
        }
    
        public void testGuice()
        {
            service.sayHello();//usage of the service
        }
    }

# Run with Gradle

To quicky setup and run with Gradle 2.2.+ and Java 8:

 1. Install gradle if not already installed
 2. Create an empty directory and navigate into it with a gradle enabled shell
 3. Create an empty java project:

    `gradle init --type java-library`


4. In the automatically generated `build.gradle`:

- change `apply plugin: 'java'` to `apply plugin: 'application'`
- add the following line

    `mainClassName = 'Main'`

- in the dependencies section add a dependency to a version of guice, e.g.:

      dependencies {
        ...
        compile group: 'com.google.inject', name: 'guice', version: '4.1.0'
        ...
      }

5. Add the classes shown above into the default package in `src/main/java`, each in its own file
6. Run and enjoy

       ..> gradlew run
           :compileJava
           :processResources UP-TO-DATE
           :classes
           :run
           Hello, world!
    
           BUILD SUCCESSFUL

           Total time: 3.595 secs

# Run with Maven

To quicky setup and run with Maven 3+ and Java 8:

 1. Install maven if not already installed
 2. Create an empty directory and navigate into it with a maven enabled shell
 3. Create an empty java project:

    `mvn archetype:generate -DgroupId=com.example -DartifactId=guice -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false`

 4. Switch to the `guice` subdirectory
 5. In the automatically generated `pom.xml`:
 
- in the `dependencies` element add a dependency to guice:


    <dependency>
       <groupId>com.google.inject</groupId>
       <artifactId>guice</artifactId>
       <version>4.1.0</version>
    </dependency>

- add the following plugin to your project (allows an easy test run)


    <project>
      .....
      <build>
            <plugins>
                <plugin>
                    <groupId>org.codehaus.mojo</groupId>
                    <artifactId>exec-maven-plugin</artifactId>
                    <version>1.5.0</version>
                    <configuration>
                        <mainClass>Main</mainClass>
                    </configuration>
                </plugin>
            </plugins>
        </build>
    </project>

6. Add the classes shown above into the default package in `src/main/java`, each in its own file
7. Run and enjoy


    ...\guice>mvn exec:java
    [INFO] Scanning for projects...
    [INFO]
    [INFO] ------------------------------------------------------------------------
    [INFO] Building guice 1.0-SNAPSHOT
    [INFO] ------------------------------------------------------------------------
    [INFO]
    [INFO] --- exec-maven-plugin:1.5.0:java (default-cli) @ guice ---
    Hello, world!
    [INFO] ------------------------------------------------------------------------
    [INFO] BUILD SUCCESS
    [INFO] ------------------------------------------------------------------------
    [INFO] Total time: 0.800 s
    [INFO] Finished at: 2016-10-09T11:44:41+03:00
    [INFO] Final Memory: 10M/309M
    [INFO] ------------------------------------------------------------------------

  [1]: http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.google.inject%22%20AND%20a%3A%22guice%22






