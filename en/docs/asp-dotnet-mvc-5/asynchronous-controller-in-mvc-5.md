---
title: "Asynchronous Controller in MVC 5"
slug: "asynchronous-controller-in-mvc-5"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Defination
Using an Asynchronous Controller in ASP.NET MVC. The AsyncController class enables you to write asynchronous action methods. You can use asynchronous action methods for long-running, non-CPU bound requests. This avoids blocking the Web server from performing work while the request is being processed.

## Asynchronous Controller
    public async Task<actionresult> Index()
    {
         return  View("View", await db.UserMasers.ToListAsync());
    }

