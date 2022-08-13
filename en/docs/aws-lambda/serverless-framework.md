---
title: "Serverless Framework"
slug: "serverless-framework"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

The open-source, application framework to easily build serverless architectures on AWS Lambda & more. This section includes, how to setup serverless framework for application development with relevant examples.

"Serverless" is a framework: https://serverless.com/

## Create Simple CRUD Operation
Create simple CRUD Operation Using Serverless Framework

 

Install Serverless framework globally <br>

    npm install serverless -g

Create simple Lambda Service <br>

    serverless create --template aws-nodejs --path myService

Go to the myService Directory it should contain<br>

 1. serverless.yml
 2. handler.js
 3. event.json
<br>

All Serverless service configuration is managed by serverless.yml<br> 
Change its contents to define CRUD service.<br>

Example serverless.yml file <br>

    service: serverless-crud
    
    provider:
      name: aws
      runtime: nodejs4.3
      region: us-east-1
      stage: dev
      iamRoleStatements:
        - Effect: Allow
          Action:
            - dynamodb:DescribeTable
            - dynamodb:Query
            - dynamodb:Scan
            - dynamodb:GetItem
            - dynamodb:PutItem
            - dynamodb:UpdateItem
            - dynamodb:DeleteItem
          Resource: "arn:aws:dynamodb:us-east-1:*:*"
    
    functions:
      create:
        handler: handler.create
        events:
          - http:
              path: todos
              method: post
              cors: true
      readAll:
        handler: handler.readAll
        events:
          - http:
              path: todos
              method: get
              cors: true
      readOne:
        handler: handler.readOne
        events:
          - http:
              path: todos/{id}
              method: get
              cors: true
      update:
        handler: handler.update
        events:
          - http:
              path: todos/{id}
              method: put
              cors: true
      delete:
        handler: handler.delete
        events:
          - http:
              path: todos/{id}
              method: delete
              cors: true
    
    resources:
      Resources:
        TodosDynamoDbTable:
          Type: 'AWS::DynamoDB::Table'
          DeletionPolicy: Retain
          Properties:
            AttributeDefinitions:
              -
                AttributeName: id
                AttributeType: S
            KeySchema:
              -
                AttributeName: id
                KeyType: HASH
            ProvisionedThroughput:
              ReadCapacityUnits: 1
              WriteCapacityUnits: 1
            TableName: 'todos' 

This file define 
 

 1. Lambda function programming Language
 2. Lambda function execution policy
 3. Dynamodb table creation and it's policy
 4. HTTP end point ( API Gateway End Point)
<br>
Then you have to define lambda function (ex. node.js) in handler.js file you can define it.

    'use strict';
    
    const todosCreate = require('./todos-create.js');
    const todosReadAll = require('./todos-read-all.js');
    const todosReadOne = require('./todos-read-one.js');
    const todosUpdate = require('./todos-update.js');
    const todosDelete = require('./todos-delete.js');
    
    module.exports.create = (event, context, callback) => {
      todosCreate(event, (error, result) => {
        const response = {
          statusCode: 200,
          headers: {
            "Access-Control-Allow-Origin" : "*"
          },
          body: JSON.stringify(result),
        };
    
        context.succeed(response);
      });
    };
    
    module.exports.readAll = (event, context, callback) => {
      todosReadAll(event, (error, result) => {
        const response = {
          statusCode: 200,
          headers: {
            "Access-Control-Allow-Origin" : "*"
          },
          body: JSON.stringify(result),
        };
    
        context.succeed(response);
      });
    };
    
    module.exports.readOne = (event, context, callback) => {
      todosReadOne(event, (error, result) => {
        const response = {
          statusCode: 200,
          headers: {
            "Access-Control-Allow-Origin" : "*"
          },
          body: JSON.stringify(result),
        };
    
        context.succeed(response);
      });
    };
    
    module.exports.update = (event, context, callback) => {
      todosUpdate(event, (error, result) => {
        const response = {
          statusCode: 200,
          headers: {
            "Access-Control-Allow-Origin" : "*"
          },
          body: JSON.stringify(result),
        };
    
        context.succeed(response);
      });
    };
    
    module.exports.delete = (event, context, callback) => {
      todosDelete(event, (error, result) => {
        const response = {
          statusCode: 200,
          headers: {
            "Access-Control-Allow-Origin" : "*"
          },
          body: JSON.stringify(result),
        };
    


        context.succeed(response);
      });
    };

Then you have to create new files for define your CRUD functions<br>
 Create these files

 1. todos-create.js
 2. todos-read-all.js
 3. todos-read-one.js
 4. todos-update.js
 5. todos-delete.js

Then define these functions in each file. <br>

for todos-create.js <br>

    'use strict';
    
    const AWS = require('aws-sdk');
    const dynamoDb = new AWS.DynamoDB.DocumentClient();
    const uuid = require('uuid');
    
    module.exports = (event, callback) => {
      const data = JSON.parse(event.body);
    
      data.id = uuid.v1();
      data.updatedAt = new Date().getTime();
    
      const params = {
        TableName: 'todos',
        Item: data
      };
    
      return dynamoDb.put(params, (error, data) => {
        if (error) {
          callback(error);
        }
        callback(error, params.Item);
      });
    };

For todos-read-all.js <br>

        'use strict';
        
        const AWS = require('aws-sdk');
        const dynamoDb = new AWS.DynamoDB.DocumentClient();
        
        module.exports = (event, callback) => {
          const params = {
            TableName: 'todos',
          };
        
          return dynamoDb.scan(params, (error, data) => {
            if (error) {
              callback(error);
            }
            callback(error, data.Items);
          });
        };
    
    
    For todos-read-one.js <br>
    
    'use strict';
    
    const AWS = require('aws-sdk');
    const dynamoDb = new AWS.DynamoDB.DocumentClient();
    
    module.exports = (event, callback) => {
      const params = {
        TableName: 'todos',
        Key: {
          id: event.pathParameters.id
        }
      };
    
      return dynamoDb.get(params, (error, data) => {
        if (error) {
          callback(error);
        }
        callback(error, data.Item);
      });
    };

For todos-update.js<br>

    'use strict';
    
    const AWS = require('aws-sdk');
    const dynamoDb = new AWS.DynamoDB.DocumentClient();
    
    module.exports = (event, callback) => {
      const data = JSON.parse(event.body);
    
      data.id = event.pathParameters.id;
      data.updatedAt = new Date().getTime();
    
      const params = {
        TableName : 'todos',
        Item: data
      };
    
      return dynamoDb.put(params, (error, data) => {
        if (error) {
          callback(error);
        }
        callback(error, params.Item);
      });
    };

For todos-delete.js<br>

    'use strict';
    
    const AWS = require('aws-sdk');
    const dynamoDb = new AWS.DynamoDB.DocumentClient();
    
    module.exports = (event, callback) => {
      const params = {
        TableName : 'todos',
        Key: {
          id: event.pathParameters.id
        }
      };
    
      return dynamoDb.delete(params, (error, data) => {
        if (error) {
          callback(error);
        }
        callback(error, params.Key);
      });
    };


For the run these application you need install npm dependencies<br>

 1. `npm init`  npm initialization
 2. `npm install aws-sdk  --save` install aws-sdk
 3. `npm install uuid --save`

**Deployment**<br>
Now you can deploy these project<br>
   `cd myService` verify you are in project directory then you can deploy your code
 

    serverless deploy
**Use End Point**<br>
If you successfully deployed you view api gateway end pont names in your console.
<br>

Test **Create** End Point<br>

    curl -X POST https://XXXX.execute-api.region.amazonaws.com/dev/todos --data '{ "body" : "Learn Serverless" }'

Test for **Read** End Point (Read All)<br>

    curl https://XXXX.execute-api.region.amazonaws.com/dev/todos
Test for  **Read** End Point (Read One) <br>

    curl https://XXXX.execute-api.region.amazonaws.com/dev/todos/<id>
Test for **Update** End Point

    curl -X PUT https://XXXX.execute-api.region.amazonaws.com/dev/todos/<id> --data '{ "body" : "Understand Serverless" }'

Test for **Delete** End Point<br>

    `curl -X DELETE https://XXXX.execute-api.region.amazonaws.com/dev/todos/<id`>





## Serverless
Install serverless globally
```
npm install serverless -g
```
Create an AWS Lamdba function in Node.js
```
serverless create --template aws-nodejs
```

Example of a `handler.js`

```js
'use strict';

// Your first function handler
module.exports.hello = (event, context, cb) => cb(null,
  { message: 'Go Serverless v1.0! Your function executed successfully!', event }
);

// You can add more handlers here, and reference them in serverless.yml
```
Deploy to live AWS account
```
serverless deploy
```

