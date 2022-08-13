---
title: "Getting started with swagger"
slug: "getting-started-with-swagger"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction - Installation - Setup (Developing in Node.js)
**Introduction:**

Swagger is a set of rules/specifications for a format describing REST APIs. It provides a powerful and actively developed ecosystem of tools around this formal specification like code generators and editors. The best part of Swagger is that the documentation of methods, parameters, and models are tightly integrated into the server code, allowing APIs to always stay in sync.
Here’s a link giving a brief overview of what is swagger: getting-started.

**Writing specifications:**

The specifications can be written in either JSON or YAML. And so we make the swagger.json or swagger.yaml file accordingly. The online editor can be used for creating the file.
Here’s a link describing the syntax for specifications: http://swagger.io/specification/

**Ways to use swagger:**

1. *API-first approach (Top down approach):* Use swagger editor → Write swagger definitions → Use swagger-codegen and swagger-ui to generate APIs
2. *Service first approach (Bottom up approach):* Develop JAX-RS resource classes using swagger annotations → Use swagger-core to automatically generate the swagger definitions → Using swagger-codegen and swagger-ui to generate client APIs and documentations.
The above can be done during maven build during swagger maven plugin.

**Installation and Setup**

In this section, we will install swagger, setup the swagger UI and generate server side and client SDK using it. For installing swagger using Node package manager execute the following command:

`npm install -g swagger`

Use of '-g' flag will ensure the module is installed globally. Next, we will create a project using the following command:

`swagger project create <project-name>`

This will ask the user to select a framework for developing the REST APIs. Express can be selected for the same. This will create the project directory with following items and a README.md file in each of them:

 - api/
    - controllers/
    - helpers/
    - mocks/
    - swagger/
 - config/
 - test/
    - api/
        - controllers/
        - helpers
 - app.js
 - package.json

The server is basically ready now and can be started using this command to be executed in project root:

`swagger project start`

If the host server is set as `localhost` and port number is not modified in `app.js` file, then the server is started at: `http://localhost:10010` Now the swagger UI can be used to further develop our REST APIs. This can be started in a new terminal using:

`swagger project edit`

This will open up the swagger editor in a browser tab on a randomly generated port. A sample hello GET request can be seen already present in the swagger.yaml file. Any further change to this file will cause the server to restart on its own. 

In the paths section, the value used for `x-swagger-router-controller` should be the javascript file name in controllers folder. As a sample, hello_world.js should be present in the controllers directory. Also, the value for `operationId` parameter represents the function name in the above javascript file. This is where business logic should be written. Thus, our swagger setup is complete and can be used to further develop our API.

