---
title: "Assets"
slug: "assets"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Accessing Assets on the Server
Static server assets must be placed in the `private` directory.

# Text files

Text files can be accessed by using the **`Assets.getText(assetPath, [asyncCallback])`** method. For example, the following JSON file is named `my_text_asset.json` and is located in the `private` directory:

    {
        "title": "Meteor Assets",
        "type": "object",
        "users": [{
            "firstName": "John",
            "lastName": "Doe"
        }, {
            "firstName": "Jane",
            "lastName": "Doe"
        }, {
            "firstName": "Matthias",
            "lastName": "Eckhart"
        }]
    }

You can access this file on the server by using the following code:

    var myTextAsset = Assets.getText('my_text_asset.json');
    var myJSON = JSON.parse(myTextAsset);
    console.log(myJSON.title); // prints 'Meteor Assets' in the server's console


# Binary files

If you want to access assets on the server as an EJSON binary, use the **`Assets.getBinary(assetPath, [asyncCallback])`** method. Here's a code example for accessing an image named `my_image.png` which is located in the `private/img` directory:

    var myBinaryAsset = Assets.getBinary('img/my_image.png');
    

