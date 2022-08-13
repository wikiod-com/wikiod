---
title: "Getting started with spock"
slug: "getting-started-with-spock"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Spock framework information can be found at the [Spock][1] website.

There are basically three ways to use Spock in Groovy

 1. as a dependency using the Grape dependency manager :

Add the following to your groovy script.

    @Grab(group='org.spockframework', module='spock-core', version='1.1-groovy-2.4.1')

or in shorthand

    @Grab('org.spockframework:spock-core:1.1-groovy-2.4.1')

2. as a maven dependency using the Gradle build tool (build.gradle)

Add the following dependency to the build.gradle file under dependencies

    ...
    dependencies {
        // mandatory dependencies for using Spock
        compile "org.codehaus.groovy:groovy-all:2.4.1"
        testCompile "org.spockframework:spock-core:1.0-groovy-2.4"
    }
    ...

3. Adding the spock-core library to your lib path

Adding the spock-core-1.0-groovy-2.4.jar to a location in your [classpath][2] where groovy can find it.


and last but not least you need to import the library so that it can be used in your groovy script

    import spock.lang.*

After you installed spock then try one of the hello world examples.

  [1]: http://spockframework.org/
  [2]: https://www.wikiod.com/java/the-classpath


## "Hello World" using when and then or expect
    import spock.lang.*
    
    class HelloWorldSpec extends Specification {
    
        @Shared message = 'Hello world!'
    
        def "The world can say hello using when and then"() {
            when:
                def newMessage = message
            then:   
                newMessage == 'Hello world!'
        }
    
        def "The world can say hello using expect"(){
            expect:
                message == 'Hello world!'
        }
    }

