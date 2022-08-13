---
title: "Getting started with aws-lambda"
slug: "getting-started-with-aws-lambda"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
1. Log into your AWS Console and click on **Lambda** under the
    **Services** tab.

2. Under **Functions** you'll be able to **Create a Lambda function** using the same-labeled button.
[![enter image description here][1]][1]

3. You'll be shown a screen where you can select a blueprint. These are simply starting points to existing Lambda functions for quickly starting out with Lambda.
[![enter image description here][2]][2]

4. On the next screen you can configure any triggers you'd like to use to "set" this Lambda function off. You can choose between no triggers (through manual setup later depending on your task), API Gateway (for creating a serverless REST client), Alexa Skills, or a plethora of other others to fire off the function you'll create.
[![enter image description here][3]][3]

5. You'll finish configuration on the next screen by setting the **Name**, **Description**, choosing a **Runtime**, opting to edit the function inline, upload a .zip file, or upload a file from Amazon S3, choose a **Role** (useful for permissions-based interaction between AWS services), set the memory and limits, and ready your app for live use.
[![enter image description here][4]][4]
[![enter image description here][5]][5]

6. Lastly, you'll review your function and create it. Since Lambda utilizes the Pay-Per-Use model, no chargers are incurred until you start using your newly created function.
[![enter image description here][6]][6]


  [1]: http://i.stack.imgur.com/nlYn9.jpg
  [2]: http://i.stack.imgur.com/pktAb.jpg
  [3]: http://i.stack.imgur.com/8Bykj.jpg
  [4]: http://i.stack.imgur.com/phQxo.jpg
  [5]: http://i.stack.imgur.com/88dQG.jpg
  [6]: http://i.stack.imgur.com/KpTTs.jpg

## Java AWS-Lambda S3 Triggered
An AWS-Lambda function can be attached to a certain bucket event. Whenever a file/folder is created or removed, an event can trigger lambda function execution.

**A simple Lambda function to print the name of an uploaded File**

 This is a one class lambda project to print the name of an uploaded file.
For maven we need to add those dependencies:

        <dependencies>
        <dependency>
            <groupId>com.amazonaws</groupId>
            <artifactId>aws-lambda-java-core</artifactId>
            <version>1.1.0</version>
            <type>jar</type>
        </dependency>
        <dependency>
            <groupId>com.amazonaws</groupId>
            <artifactId>aws-lambda-java-events</artifactId>
            <version>1.3.0</version>
            <type>jar</type>
        </dependency>
    </dependencies>

Now let's go to our HelloWorld Class:

    package com;
    import com.amazonaws.services.lambda.runtime.Context;
    import com.amazonaws.services.lambda.runtime.RequestHandler;
    import com.amazonaws.services.lambda.runtime.events.S3Event;
    import com.amazonaws.services.s3.event.S3EventNotification;
    
    public class HelloWorld implements RequestHandler< S3Event, String> {
        @Override
        public String handleRequest(S3Event event, Context ctx) {
         S3EventNotification.S3EventNotificationRecord record=event.getRecords().get(0);
         System.out.println("Bucket Name is "+record.getS3().getBucket().getName());
         System.out.println("File Path is "+record.getS3().getObject().getKey());
         return null;
        }
    }

Next step is to build the project using mvn.

After building the project, we need to upload it to AWS-Lambda.  Go to Lambda, choose "create a lambda function". Skip the part where you choose the blueprint, because Java is usually not there.

Also, skip "Configure triggers" because we will configure it from a different location. The next page, enter a name for your first lambda function, then a small description and choose Java as runtime.

For the "Code entry type", choose "Upload from a .ZIP file" and then select your .zip file in the next location to upload it. 

The tricky part on this page is the Handler field. In the handler field, you have to specify the location of the class the implements the RequestHandler. This class is the entry point for the lambda and your Lambda function won't work if this is not specified correctly. For our case handler is "com.HelloWorld"

**Attaching a S3 trigger to Lambda:**

Here we will attach a trigger to the S3 file upload
 - Go to S3, choose the bucket, then "Properties".
 - In the "Properties" section, go to "Events".
 - Add event details. In the "Events" field, choose how you want to trigger your Lambda. We will choose "ObjectCreated(All)"
  Note that the lambda function and the bucket need to be on the same amazon Region
 - For "Sends to", check Lambda Function, and choose your lambda function from the list.

**Monitoring Lambda Output**

Now, we will upload a file to the bucket that has the lambda trigger. To see the lambda outputs and logs, go to "CloudWatch", then choose "Logs", then choose your Lambda function. You might see many entries under "Log Streams", choose the latest one and open it. You should be able to see the output of the lambda execution there.

