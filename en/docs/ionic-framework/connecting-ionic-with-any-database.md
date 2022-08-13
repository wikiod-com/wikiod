---
title: "Connecting Ionic with any database"
slug: "connecting-ionic-with-any-database"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## You can't do it directly from Ionic framework
The thing is; you can't connect Ionic to any database (MySQL, Postgres, MSSQL, ...) **directly**. The keyword here is _directly_.

No, there's no workaround, no magic involved, it's just not the way this is supposed to work. Ionic works on top of Angular and Angular is a frontend framework.

However, the way you should do it is that you basically create a (RESTful) API on your server side.

Most likely this will be made with some serverside language (PHP, Go, Python, ...) which will talk directly to your database and query it.

After you write your (RESTful) API you can consume it through your services in Angular by using Angular's [`$resource`](https://docs.angularjs.org/api/ngResource/service/$resource) or [`$http`](https://docs.angularjs.org/api/ng/service/$http) service.

An example of consuming [Giphy API]() with Angular's [`$http`](https://docs.angularjs.org/api/ng/service/$http) service:

    var search = 'cats';
    var link = 'http://api.giphy.com/v1/gifs/search?api_key=dc6zaTOxFJmzC&amp;q=' + search;
     
    $http.get(link).then(function(result){
        console.log(result);
    });

