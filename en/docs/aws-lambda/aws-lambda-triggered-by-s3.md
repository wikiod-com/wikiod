---
title: "aws-lambda triggered by S3"
slug: "aws-lambda-triggered-by-s3"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Syntax
- Deserialized JSON based object
- "Records" key has one or more actual events
- Each sub event object contains all information you need to determine what changed

## Parameters
| Parameter | Details |
|-----------|---------|
| `Records` -> `[]` -> `s3` -> `bucket` -> `name` | The name of the S3 bucket |
| `Records` -> `[]` -> `s3` -> `object` -> `key` | The path and name of the file. |



## File names

Unlike most file paths, the S3 key name (JSON Schema: `$.Records[0].s3.object.key`) does not include a leading slash. So, if you have a file in the path `s3://mybucket/path/file.txt`, the key will be `path/file.txt`

In Python at least, the key field value is UTF-8 URL encoded. This is noticeable when the filename contains spaces or non-ascii characters. The field needs to be URL decoded, then UTF-8 decoded - See http://stackoverflow.com/questions/39465220/get-non-ascii-filename-from-s3-notification-event-in-lambda

## Records key

It is possible to have multiple of the same (or different) actions inside of the `"Records"` key of the event; however, in practice, you will usually see one event per invocation of your Lambda function.


## More Examples & Testing

There are actually sample events in the Lambda console, if you choose Actions -> Configure Test Event. However, You can see the PUT operation in the examples above.

You can modify and submit test events from the AWS Lambda console to see how your function responds.

## S3 PUT Operation
    {
      "Records": [
        {
          "eventVersion": "2.0",
          "eventTime": "1970-01-01T00:00:00.000Z",
          "requestParameters": {
            "sourceIPAddress": "127.0.0.1"
          },
          "s3": {
            "configurationId": "testConfigRule",
            "object": {
              "eTag": "0123456789abcdef0123456789abcdef",
              "sequencer": "0A1B2C3D4E5F678901",
              "key": "HappyFace.jpg",
              "size": 1024
            },
            "bucket": {
              "arn": "arn:aws:s3:::mybucket",
              "name": "sourcebucket",
              "ownerIdentity": {
                "principalId": "EXAMPLE"
              }
            },
            "s3SchemaVersion": "1.0"
          },
          "responseElements": {
            "x-amz-id-2": "EXAMPLE123/5678abcdefghijklambdaisawesome/mnopqrstuvwxyzABCDEFGH",
            "x-amz-request-id": "EXAMPLE123456789"
          },
          "awsRegion": "us-east-1",
          "eventName": "ObjectCreated:Put",
          "userIdentity": {
            "principalId": "EXAMPLE"
          },
          "eventSource": "aws:s3"
        }
      ]
    }

