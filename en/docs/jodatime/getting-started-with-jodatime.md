---
title: "Getting started with jodatime"
slug: "getting-started-with-jodatime"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction
[Joda-Time][1] is a robust alternative to the Java date and time classes. 

Prior to Java SE 8, the standard Java date and time classes like [`java.util.Calendar`][2] are difficult to use and prone to errors. Joda-Time emerged as the de-facto standard library for date and time manipulation in many open-source-projects.

However, starting with Java SE 8 the package `java.time` (JSR-310) is available and users are asked to migrate to the same since Joda-Time is now in [maintenance mode][5].

# When to use Joda-Time

You want to manipulate dates and times **and**:
 1. You are developing a project in an environment where Java SE8 is not available
 2. You are maintaining a legacy project that already uses Joda-Time
 3. You are developing a cross-platform project and you would like to maintain an API which has similarities to the APIs of other libraries like [Noda Time][3] and [js-joda][4] (although there is no exact match).

# When not to use Joda-Time
 1. You don't need to work with dates and times
 2. You are developing a new project where Java SE8 is available: instead use the `java.time` (JSR-310) classes.

# Considerations for using Joda-Time in Android apps

Since the standard Joda-Time library can inflate the memory-footprint of apps, consider using [joda-time-android][6]. This is a fork optimized for Android development, and also contains a Joda-Time port of Android's native [`DateUtils`][7].

[1]:http://www.joda.org/joda-time/
[2]:https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
[3]:http://nodatime.org/
[4]:https://js-joda.github.io/js-joda/
[5]:https://en.wikipedia.org/wiki/Maintenance_mode
[6]:https://github.com/dlew/joda-time-android
[7]:https://developer.android.com/reference/android/text/format/DateUtils.html

## Installation
# Using the library archive 

Download the [JAR][1] and add it to the classpath for your Java project

# Using a build tool

If you are using a build tool like Maven or Gradle:

 1. Maven

        <dependency>
            <groupId>joda-time</groupId>
            <artifactId>joda-time</artifactId>
            <version>2.9.6</version>
        </dependency>

2. Gradle

    Add the following line to the `dependencies` closure inside your `build.gradle`:

        compile 'joda-time:joda-time:2.9.6'

[1]:https://repo1.maven.org/maven2/joda-time/joda-time/2.9.6/joda-time-2.9.6.jar


## Hello Joda!
We can now write the following HelloJoda program!


    import org.joda.time.LocalDate;
    
    public class HelloJoda {
    
        public static void main(String [] args) {
            LocalDate today = LocalDate.now();
            System.out.println("Hello Joda! Today's date is: " + today);
        }
    }

Which will output something like this:

    Hello Joda! Today's date is: 2016-11-26


