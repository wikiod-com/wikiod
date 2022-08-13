---
title: "Maven Surefire Plugin"
slug: "maven-surefire-plugin"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - mvn test
 - mvn -Dtest=com.example.package.ExampleTest test




## Testing a Java class with JUnit and the Maven Surefire plugin
The Maven Surefire plugin runs during the test phase of the Maven build process or when `test` is specified as a Maven goal. The following directory structure and minimum `pom.xml` file will configure Maven to run a test.

Directory structure inside the project's root directory:

    ─ project_root 
      ├─ pom.xml
      ├─ src
      │  ├─ main
      │  │  └─ java
      │  └─ test
      │     └─ java
      └─ target
         └─ ...

`pom.xml` contents:

    <project>
      <modelVersion>4.0.0</modelVersion>
      <groupId>com.example</groupId>
      <artifactId>company-app</artifactId>
      <version>0.0.1</version>
      <dependencies>
        <dependency>
          <groupId>junit</groupId>
          <artifactId>junit</artifactId>
          <version>4.11</version>
          <scope>test</scope>
        </dependency>
      </dependencies>
    </project>

Create a file called `PlusTenTest.java` with the following contents in the project's `src/test/java/com/example/app` directory:

    package com.example.app;

    import static org.junit.Assert.assertEquals;
    import org.junit.Test;

    public class PlusTenTest {
        
        @Test
        public void incrementTest() {
            int result = PlusTen.increment(10);
            assertEquals("PlusTen.increment(10) result", 20, result);
        }
    }

The annotation `@Test` tells JUnit that it should run `incrementTest()` as a test during the `test` phase of the Maven build process. Now create `PlusTen.java` in `src/main/java/com/example/app`:

    package com.example.app;
    
    public class PlusTen {
        public static int increment(int value) {
            return value;
        }
    }

Run the test by opening a command prompt, navigating to the project's root directory and invoking the following command:

    mvn -Dtest=com.example.app.PlusTenTest test

Maven will compile the program and run the test method `incrementTest()` in `PlusTenTest`. The test will fail with the following error:

    ...
    Tests run: 1, Failures: 1, Errors: 0, Skipped: 0, Time elapsed: 0.005 sec <<< FAILURE! - in com.example.app.PlusTenTest
    incrementTest(com.example.app.PlusTenTest)  Time elapsed: 0.004 sec  <<< FAILURE!
    java.lang.AssertionError: PlusTen.increment(10) result expected:<20> but was:<10>
    at org.junit.Assert.fail(Assert.java:88)
    at org.junit.Assert.failNotEquals(Assert.java:743)
    at org.junit.Assert.assertEquals(Assert.java:118)
    at org.junit.Assert.assertEquals(Assert.java:555)
    at com.example.app.PlusTenTest.incrementTest(PlusTenTest.java:12)


    Results :
    
    Failed tests:   
      PlusTenTest.incrementTest:12 PlusTen.increment(10) result expected:<20> but was:<10>
    
    Tests run: 1, Failures: 1, Errors: 0, Skipped: 0
    
    [INFO] ------------------------------------------------------------------------
    [INFO] BUILD FAILURE
    [INFO] ------------------------------------------------------------------------
    [INFO] Total time: 2.749 s
    [INFO] Finished at: 2016-09-02T20:50:42-05:00
    [INFO] Final Memory: 14M/209M
    [INFO] ------------------------------------------------------------------------
    [ERROR] Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:2.19.1:test (default-test) on project app: There are test failures.
    ...

The Maven Surefire plugin creates a `/target/surefire-reports/` directory in your project's directory containing the files `com.example.app.PlusTenTest.txt` and `TEST-com.example.app.PlusTenTest.xml` that contain the error details of the beginning of the output above.

Following the test-driven development pattern, modify `PlusTen.java` so that the `increments()` method works correctly:

    package com.example.app;
    
    public class PlusTen {
        public static int increment(int value) {
            return value + 10;
        }
    }

Invoke the command again:

    mvn -Dtest=com.example.app.PlusTenTest test

The test passes:

    -------------------------------------------------------
     T E S T S
    -------------------------------------------------------
    Running com.example.app.PlusTenTest
    Tests run: 1, Failures: 0, Errors: 0, Skipped: 0, Time elapsed: 0.028 sec
    
    Results :
    
    Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
    
    [INFO] ------------------------------------------------------------------------
    [INFO] BUILD SUCCESS
    [INFO] ------------------------------------------------------------------------
    [INFO] Total time: 2.753 s
    [INFO] Finished at: 2016-09-02T20:55:42-05:00
    [INFO] Final Memory: 17M/322M
    [INFO] ------------------------------------------------------------------------

Congratulations! You have tested a Java class using JUnit and the Maven Surefire plugin.

