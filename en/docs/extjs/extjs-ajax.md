---
title: "ExtJS AJAX"
slug: "extjs-ajax"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

A singleton instance of an [`Ext.data.Connection`][1] class. This class is used to communicate with your server side.


[1]:http://docs.sencha.com/extjs/6.0.1/classic/src/Connection.js.html#Ext.data.Connection

## Basic Request

Some of the Class properties `Ext.Data.Connection`

| Properties | Details |
| --------- | ------- |  
| `url` | Address of the request |
| `timeout` | Waiting time in milliseconds | 
| `success` | Return on success |
| `failure` | Return on failure |


    Ext.Ajax.on("beforerequest", function(conn , options , eOpts) {
        console.log("beforerequest");
    });
    Ext.Ajax.on("requestcomplete", function(conn , response , options , eOpts) {
        console.log("requestcomplete");
    });
    Ext.Ajax.on("requestexception", function(conn , response , options , eOpts) {
        console.log("requestexception");
    });
    
    Ext.Ajax.request({
        url: 'mypath/sample.json',
        timeout: 60000,
        success: function(response, opts) {
            var obj = Ext.decode(response.responseText);
            console.log(obj);                
        },
        failure: function(response, opts) {
            console.log('server-side failure with status code ' + response.status);
        }
    });

