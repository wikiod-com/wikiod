---
title: "AWS Lambda using Python"
slug: "aws-lambda-using-python"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Hello World - Lambda Function
This is one of the simplest lambda function. It is equivalent to Hello World program.  
To create your first program follow the below mentioned steps. 

 1. Login to AWS Console 
 2. Click Lambda under compute 
 3. Click create a Lambda Function [![enter image description here][1]][1]
 4. Skip select blueprint section 
 5. In configure trigger click on the dotted rectangle [![enter image description here][2]][2]
 6. Select API Gateway 
 7. Fill the required details as in the image. [![enter image description here][3]][3] **API Name** is the name of your API you are going to build. **Resource Pattern** is the URL path which you can invoke your Lambda function. Select the required http method. In our example we choose GET. In AWS staging can be done in different ways like prod,dev ... It will help you to differentiate the functions from prod and dev. For demo purpose lets choose security as Open(Its not recommended in production).Click next
 8. Configure the function as below [![enter image description here][4]][4] Provide the function name , description of your function and runtime environment. We are choosing python as runtime environment. 
 9. Modify the code. [![enter image description here][5]][5] Here we are printing the aws lambda event in cloudtrail which is free. It is also returning a string. 
 10. Provide Lambda function handler and role [![enter image description here][6]][6] Make sure that the handler name should start with lambda_function.<Lambda function Name>. Also create a new role for execute the lambda function. Select the amount of main memory required for execute your function. Select the default timeout and click next 
 11. Click create function 
 12. Now your function is ready to execute. Click the link provided by the aws [![enter image description here][7]][7][![enter image description here][8]][8] When you click the link your lambda function will be executed in the background and you will get output in the browser. 


  [1]: http://i.stack.imgur.com/CBEBZ.png
  [2]: http://i.stack.imgur.com/Zva8Z.png
  [3]: http://i.stack.imgur.com/1ERDk.png
  [4]: http://i.stack.imgur.com/vigbD.png
  [5]: http://i.stack.imgur.com/tgyX3.png
  [6]: http://i.stack.imgur.com/O6U8b.png
  [7]: http://i.stack.imgur.com/XBdiS.png
  [8]: http://i.stack.imgur.com/slO8B.png

## Why AWS Lambda ?
AWS Lambda supports 

 - Transparent scalability and availability
 - Developer friendly operations and no need to manage servers
 - Native integration to AWS services
 - No need to pay for idle time 
 - RESTful integration 
 - Monitoring the RESTful interface using AWS API gateway  
 

