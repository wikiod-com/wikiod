---
title: "using https with express"
slug: "using-https-with-express"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Using https with express
First you have to generate public and private keys using OpenSSL([tutorial][1]).


    var express = require("express");
    var http =require ("http"); 
    var https=require ("https");
    var fs=require("fs");
    var app=express();
    var httpsKeys={
    key:fs.readFileSync("<key.pem>");
    crtifcte:fs.readFileSync("<certificate.pem>");
    };
    http.createserver(app).listen(3000);
    https.createserver(httpsKeys,app).listen(3030);


  [1]: https://matoski.com/article/node-express-generate-ssl/

