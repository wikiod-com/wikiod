---
title: "Using Pax Exam for integration testing OSGi applications"
slug: "using-pax-exam-for-integration-testing-osgi-applications"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

**[Pax Exam](https://ops4j1.jira.com/wiki/display/PAXEXAM4/Pax+Exam)** allows testing of bundles within an OSGi container (e.g. AEM, Apache Karaf). Pax Exam is usually used in conjunction with JUnit.

## Getting Started
Here is an example of a test using Pax Exam. 

    package com.example.project.test;
 
    import static org.junit.Assert.*;
    import static org.ops4j.pax.exam.CoreOptions.*;
     
    import javax.inject.Inject;
     
    import org.junit.Test;
    import org.junit.runner.RunWith;
    import org.ops4j.pax.exam.Configuration;
    import org.ops4j.pax.exam.Option;
    import org.ops4j.pax.exam.junit.PaxExam;
    import org.ops4j.pax.exam.regression.pde.HelloService;
    import org.ops4j.pax.exam.spi.reactors.*;
      
    @RunWith(PaxExam.class)
    @ExamReactorStrategy(PerMethod.class)
    public class SampleTest {
     
        @Inject
        private HelloService helloService;
     
        @Configuration
        public Option[] config() {
     
            return options(
                mavenBundle("com.example.myproject", "myproject-api", "1.0.0-SNAPSHOT"),
                bundle("http://www.example.com/repository/foo-1.2.3.jar"),
                junitBundles()
                );
        }
     
        @Test
        public void getHelloService() {
            assertNotNull(helloService);
            assertEquals("Hello Pax!", helloService.getMessage());
        }
    }

Code is from [Pax Exam 4 Page](https://ops4j1.jira.com/wiki/display/PAXEXAM4/Getting+Started+with+OSGi+Tests)

