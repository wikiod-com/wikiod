---
title: "Getting started with rx-java"
slug: "getting-started-with-rx-java"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## An introduction to RxJava
The core concepts of RxJava are its `Observables` and `Subscribers`. An `Observable` emits objects, while a `Subscriber` consumes them. 

**Observable**

`Observable` is a class that implements the reactive design pattern. These Observables provide methods that allow consumers to subscribe to event changes. The event changes are triggered by the observable. There is no restriction to the number of subscribers that an `Observable` can have, or the number of objects that an `Observable` can emit. <br>

Take for example:

    Observable<Integer> integerObservable = Observable.just(1, 2, 3); // Integer observable
    Observable<String> stringObservable = Observable.just("Hello, ", "World", "!"); // String observable

Here, an observable object called `integerObservable` and `stringObservable` are created from the factory method `just` provided by the Rx library. Notice that `Observable` is generic and can thus can emit any object.

**Subscriber**

A `Subscriber` is the consumer. A `Subscriber` can subscribe to ***only one*** observable. The `Observable` calls the `onNext()`, `onCompleted()`, and `onError()` methods of the `Subscriber`.

    Subscriber<Integer> mSubscriber = new Subscriber<Integer>() {
            // NOTE THAT ALL THESE ARE CALLED BY THE OBSERVABLE
            @Override
            public void onCompleted() {
                // called when all objects are emitted
                System.out.println("onCompleted called!");
            }

            @Override
            public void onError(Throwable throwable) {
                // called when an error occurs during emitting objects
                System.out.println("onError called!");
            }

            @Override
            public void onNext(Integer integer) {
                // called for each object that is emitted
                System.out.println("onNext called with: " + integer);
            }
        };
Notice that `Subscriber` is also generic and can support any object. A `Subscriber` must subscribe to the observable by calling the `subscribe` method on the observable.

    integerObservable.subscribe(mSubscriber);

The above, when run, will produce the following output:

    onNext called with: 1
    onNext called with: 2
    onNext called with: 3
    onCompleted called!



## Installation or Setup
rx-java set up

 1. Gradle

        compile 'io.reactivex:rxjava2:rxjava:2.1.1'

 2. Maven

        <dependency>
            <groupId>io.reactivex.rxjava2</groupId>
            <artifactId>rxjava</artifactId>
            <version>2.1.1</version>
        </dependency>

 3. Ivy

        <dependency org="io.reactivex.rxjava2" name="rxjava" rev="2.1.1" />

 4. Snapshots from JFrog

        repositories {
        maven { url 'https://oss.jfrog.org/libs-snapshot' }
        }
        
        dependencies {
            compile 'io.reactivex:rxjava:2.0.0-SNAPSHOT'
        }

 5. If you need to download the jars instead of using a build system, create a Maven `pom` file like this with the desired version:

        <?xml version="1.0"?>
        <project xmlns="http://maven.apache.org/POM/4.0.0" 
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
            xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
            <modelVersion>4.0.0</modelVersion>
            <groupId>com.netflix.rxjava.download</groupId>
            <artifactId>rxjava-download</artifactId>
            <version>1.0-SNAPSHOT</version>
            <name>Simple POM to download rxjava and dependencies</name>
            <url>http://github.com/ReactiveX/RxJava</url>
            <dependencies>
                <dependency>
                    <groupId>io.reactivex</groupId>
                    <artifactId>rxjava</artifactId>
                    <version>2.0.0</version>
                    <scope/>
                </dependency>
            </dependencies>
        </project>
Then execute:

    $ mvn -f download-rxjava-pom.xml dependency:copy-dependencies
That command downloads `rxjava-*.jar` and its dependencies into `./target/dependency/.`

You need Java 6 or later.

## Hello, World!
The following prints the message `Hello, World!` to console

<!-- language: lang-java -->
    public void hello() {
      Observable.just("Hello, World!") // create new observable
        .subscribe(new Action1<String>() { // subscribe and perform action

           @Override
           public void call(String st) {
             System.out.println(st);
           }

        });
    }

Or using Java 8 lambda notation

````
public void hello() {
      Observable.just("Hello, World!") // create new observable
        .subscribe(onNext -> { // subscribe and perform action
             System.out.println(onNext);   
        });
}
````

## Understanding Marble Diagrams
An Observable can be thought of as just a stream of events. When you define an Observable, you have three listeners: onNext, onComplete and onError. onNext will be called every time the observable acquires a new value. onComplete will be called if the parent Observable notifies that it finished producing any more values. onError is called if an exception is thrown any time during the execution of the Observable chain. To show operators in Rx, the marble diagram is used to display what happens  with a particular operation. Below is an example of a simple Observable operator "Just."

[![Example of a marble diagram][1]][1]

Marble diagrams have a horizontal block that represents the operation being performed, a vertical bar to represent the completed event, a X to represent an error, and any other shape represents a value. With that in mind, we can see that "Just" will just take our value and do an onNext and then finish with onComplete. There are a lot more operations then just "Just." You can see all the operations that are part of the ReactiveX project and there implementations in RxJava at the [ReativeX site][2]. There are also interactive marble diagrams via [RxMarbles site][3].


  [1]: http://i.stack.imgur.com/Mixu4.png
  [2]: http://reactivex.io/documentation/operators.html
  [3]: http://rxmarbles.com/

