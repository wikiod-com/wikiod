---
title: "Getting started with asynchronous"
slug: "getting-started-with-asynchronous"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting asynchronous set up or installed.

## File Reading in Node js
          
          var fs = require("fs");
          fs.readFileSync(‘abc.txt’,function(err,data){ //Reading File Synchronously
          if(!err) {
          console.log(data);
          }
          //else
          //console.log(err);  
          });
          console.log("something else");


Here, the program was waiting while reading the file. It will not go further before completing the read operation, which is an example of blocking code. But ideally, we should proceed further while the program was reading the file and once it is done we should go back and process that. That is what happening in the following code.


        var fs = require("fs");
        fs.readFile(‘abc.txt’,function(err,data){//Reading file Asynchronously
        if(!err) {
        console.log(data);
        }
        });
        console.log("something else");

Here, the program is not waiting, hence you see the console first and file contents later.

