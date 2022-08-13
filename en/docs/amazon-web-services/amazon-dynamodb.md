---
title: "Amazon DynamoDB"
slug: "amazon-dynamodb"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## DynamoDB basic Crud Operation using NodeJS
<pre><code>
let doc = require('dynamodb-doc');
let dynamo = new doc.DynamoDB();
var tblName = "MyTable";

exports.handler = (event, context, callback) => {
    readOperation(context);
}

function readOperation(cnxt) {
    var params = {
        TableName: tblName,
        Key: {
            "id": "2013",
            "topic": "Turn It Down, Or Else!"
        },
        AttributesToGet: [
            "id", "client_name", "info"
        ],
        ConsistentRead: false
    };
    dynamo.getItem(params, function(err, data) {
        if (err) console.log("Error: "+err); // an error occurred
        else {
            var jsonDoc = JSON.parse(data.Item.info);   // successful response
            cnxt.succeed(jsonDoc);
        }
    });
}
</code>
</pre>


