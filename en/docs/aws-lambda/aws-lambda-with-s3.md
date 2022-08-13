---
title: "AWS Lambda with S3"
slug: "aws-lambda-with-s3"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Lambda developers will handle issues that requires the use of other AWS resources. This topic focuses on S3 (Simple Storage Service) which will commonly be used for storing static files and other configurations. This documentation will consider using AWS-SDK in lambda, accessing files in S3 from Lambda and triggering Lambda functions when an S3 event gets fired 

## Cheatsheet
<h2>AWS-SDK for javascript</h2>
Lambda contains aws-sdk (https://aws.amazon.com/sdk-for-node-js/) in its global so no need to upload this node-module into the zip.
<pre>
const AWS = require('aws-sdk');
</pre>

<h2>Sample function</h2>

<pre>
module.exports.myFunction = (event, context, callback) => {
    const response = {
        statusCode: 200,
        body: 'Hello Lambda!',
    };
    return callback(null, response);
};
</pre>

<h2>Running S3</h2>

const s3 = new AWS.S3();

<h2>Use with Elasticache Redis</h2>
<pre>
//make sure redis node-module is added in zip
const redis = require('redis'); 
//the redis information should be stored in the environment, not hard coded
const redis_options = {
    host: process.env.REDIS_HOST,
    port: process.env.REDIS_PORT
};

module.exports.myFunction = (event, context, callback) => {
    try {
        let client = redis.createClient(redis_options);
        context.callbackWaitsForEmptyEventLoop = false;

        client.on('connect', () => {
            console.log('Connected:', client.connected);
        });

        client.on('end', () => {
            console.log('Connection closed.');
        });

        client.on('ready', function () {
            console.log('Connection ready.');
            client.keys('*', (err, keys) => {
            //always quit the redis client when no longer needed
            //else the connection will be used up
            client.quit(); 

            const response = {
                statusCode: 200,
                body: keys,
            };
            
            return callback(null, response);
        });
    } catch (err) {
        if (client) { client.quit();}
        console.log('Error!: ' + err.message);
        callback(err);
    }
};
</pre>

