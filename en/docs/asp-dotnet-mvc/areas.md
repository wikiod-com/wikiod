---
title: "Areas"
slug: "areas"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

**What is area?**

An area is a smaller unit in MVC application which used as a way to separate large amount of application modules into functional groups. An application can contain multiple areas which stored in Areas folder.

Each area can contain different models, controllers and views depending on requirements. To use an area, it is necessary to register the area name in `RouteConfig` and define route prefix for it.


if you want to go to this area through your default controller 


  

          return RedirectToAction("Index","Home",new{area="areaname"});

## Create a new area
Right click on your project folder/name and create new area and name it.

In mvc internet/empty/basic application a folder with the name of the area will be created,which will contain three different folders named controller , model and views and a class file called

"*areaname*AreaRegistration.cs"   

## Configure RouteConfig.cs
In your App_start folder open routeconfig.cs  and do this


     routes.MapRoute(
                name: "Default",
                url: "{controller}/{action}/{id}",
                defaults: new { controller = "Home", action = "Index", id = UrlParameter.Optional },
                namespaces:new []{"nameofyourproject.Controllers"}// add this line ;
            );

## Create a new controller and configure  areanameAreaRegistration.cs maproute
Create a new controller foreg 

  ControllerName: "Home",
  ActionresultName :"Index"

open AreaRegistraion.cs and add the controller name and action name to be rerouted to

        context.MapRoute(
                "nameofarea_default",
                "nameofarea/{controller}/{action}/{id}",  // url shown will be like this in browser
                new {controller="Home", action = "Index", id = UrlParameter.Optional }
            );

