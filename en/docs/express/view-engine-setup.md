---
title: "View engine setup"
slug: "view-engine-setup"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Often the server needs to serve pages dynamically.For an example an user Mr.X visits the page and sees some thing like "Welcome Mr. X to my homepage".In this case views can be helpful.Even to populate a table view can be handy.
Variables can be injected into HTML dynamically using view engine.View engine is something that renders the views.One can keep views to be served in a folder called view and serve upon request .The path of the folder can be shown to Express using path.resolve method .

install ejs using the following(I know it's obvious)

    sudo npm install ejs --save

## 1:setting up the views
    var express=require("express");    //express is included
    var path=require("path");    //path is included 
    
    var app=express();    //app is an Express type of application
    
    app.set("views",path.resolve(__dirname,"views"));    //tells express about the location of the views in views folder
    app.set("view engine","ejs");    //tells express that ejs template engine is used
    

## 2.EJS file example(refer 1.setting up... before this)
the following is an ejs file.
        
    <!DOCTYPE html>
    <html>
        <head>
            <meta charset="utf-8">
            <title>Hello, world!</title>
        </head>
        <body>
            <%= message %>
        </body>
    </html>

## 3.rendering view with express(refer 2.EJS file... before this)
    app.get("/",function(req,res){        
    response.render("index",{                  //render the index when root(/) is requested
        message:"rendered view with ejs"
            });
    });

## 4.after rendering final HTML is created(refer 3.rendering... before this)
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8">
        <title>Hello, world!</title>
    </head>
    <body>
        message:"rendered view with ejs"
    </body>
    </html>

