---
title: "AWS Lambda"
slug: "aws-lambda"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

AWS Lambda is a service that lets you run back-end code without the need to provision or manage servers. AWS Lambda takes care of scaling and high availability. The cost directly depends on how often and how long your code executes. 

You will find examples of how to create and deploy AWS Lambda functions in different languages.

* AWS Lambda code must be written in a stateless manner. While the instance of a lambda might be retained and re-used, you must never expect this. 

## Basic Gradle Java project
To deploy Java code to AWS Lambda, you have to create a distribution zip file that contains all dependencies that are needed during the runtime. Example project in Gradle:

    apply plugin: 'java'
    
    repositories {
        mavenCentral()
    }
    
    dependencies {
        compile 'com.amazonaws:aws-lambda-java-core:1.1.0'
    }
    
    task buildZip(type: Zip) {
        from compileJava
        from processResources
        into('lib') {
            from configurations.runtime
        }
    }
    
    build.dependsOn buildZip

Running `gradle build` will create a zip file with all dependencies bundled with your code, ready to deploy.

## Basic Lambda Code in Java
A lambda needs a handler, which will serve as the entry point to your application. Every handler needs to implement interface `RequestHandler<I, O>` where `I` is the input type and `O` is the output type. The input type is then passed to the `handleRequest()` method and the method returns the output type.

A simple example could look like this:

    package com.example.lambda;
    
    import com.amazonaws.services.lambda.runtime.Context;
    import com.amazonaws.services.lambda.runtime.RequestHandler;
    
    public class HelloNumber implements RequestHandler<Integer, String> {
    
        @Override
        public String handleRequest(Integer input, Context context) {
            return "You sent " + input;
        }
    }



## Basic Lambda code in JavaScript (NodeJs)
A lambda needs a handler, which will serve as the entry point to your application. In the simplest case, you get your input from the `context` and pass the result using the `callback()` function:

exports.handler = (event, context, callback) => {
    callback(null, 'You sent ' + event.number);
};

## Creating AWS Lambda using the web GUI (Java)
To create a Java lambda using the GUI, first make sure you have your distribution zip ready. You will also need an AWS account that can create lambdas. 

 - Switch to the AWS Lambda, dismiss the introduction screen (or read the Get Started documentation) and press the button `Create a Lambda Function`.
 - Select an appropriate blueprint. A blueprint is an example template that can help you create your lambda. For now, start with a `Blank Function`.
 - For now, don't worry about triggers and skip that screen.
 - Name your function and upload the zip file. 
 - On the same configuration screen, type in your Handler name as fully qualified Java class with package, optionally with the method that will handle the requests. If you have used the Java interface from Basic Lambda Code example, the class name is enough.
 - On the same screen, select `Create a new role from template(s)` and name your role. The role is used to give your lambda access to other AWS resources. In this case, leave it empty as we don't need any other resources.
 - Select the lowest available memory (128MB). The memory influnces cost as well.
 Save your function.

## Testing the Basic Lambda code
If you have successfully deployed the Lambda, you can also test it directly in the GUI. When you click the blue `Test` button for the first time, it presents you with a creation of a new test event. 

If you are testing the Java code from the *Basic Lambda code in Java example*, delete the whole body and simply add a number `1`.

If you are testing the Javascript code from the *Basic Lambda code in Javascript (Node)* example, create a json that looks like this:

    {
        "number": "1"
    }

Run the test. You should see the result:

"You sent 1" 

## Adding AWS API Gateway trigger
Probably the most common way to invoke a Lambda is to use API Gateway, which maps a REST call to the lambda code. You can add multiple triggers to your Lambda during at any time, including when creating a new lambda.

If you are familiar with API Gateway, you can associate any method with a deployed lambda. Simply create a new method with Integration type set to Lambda function and point to your lambda. You can map both REST inputs to lambda inputs and outputs to REST outputs. 

TODO: Decide whether to describe API Gateway here or reference another part of documentation. 

