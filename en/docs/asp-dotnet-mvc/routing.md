---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9894
type: docs
toc: true
---

Routing is how ASP.NET MVC matches a URI to an action. Routing module is responsible for mapping incoming browser requests to particular MVC controller actions.

MVC 5 supports a new type of routing, called attribute routing. As the name implies, attribute routing uses attributes to define routes. Attribute routing gives you more control over the URIs in your web application.

## Attribute routing in MVC
Along with classic way of route definition MVC WEB API 2 and then MVC 5 frameworks introduced `Attribute routing`:


    public class RouteConfig
    {
        public static void RegisterRoutes(RouteCollection routes)
        {
            routes.IgnoreRoute("{resource}.axd/{*pathInfo}");
     
            // This enables attribute routing and must go  before other routes are added to the routing table.
            // This makes attribute routes have higher priority
            routes.MapMvcAttributeRoutes();  
        }
    }

For routes with same prefix inside a controller, you can set a common prefix for entire action methods inside the controller using `RoutePrefix` attribute.

    [RoutePrefix("Custom")]
    public class CustomController : Controller
    {
        [Route("Index")]
        public ActionResult Index()
        {
            ...
        }
    }

`RoutePrefix` is optional and defines the part of the URL which is prefixed to all the actions of the controller.

If you have multiple routes, you may set a default route by capturing action as parameter then apply it for entire controller unless specific `Route` attribute defined on certain action method(s) which overriding the default route.

    [RoutePrefix("Custom")]
    [Route("{action=index}")]
    public class CustomController : Controller
    {
        public ActionResult Index()
        {
            ...
        }

        public ActionResult Detail()
        {
            ...
        }
    }

## Catch-all route
Suppose we want to have a route that allows an unbound number of segments like so:

* http://example.com/Products/ (view all products)
* http://example.com/Products/IT
* http://example.com/Products/IT/Laptops
* http://example.com/Products/IT/Laptops/Ultrabook
* http://example.com/Products/IT/Laptops/Ultrabook/Asus
* etc.

We would need to add a route, normally at the end of the route table because this would probably catch all requests, like so:

    routes.MapRoute("Final", "Route/{*segments}",
          new { controller = "Product", action = "View" });

In the controller, an action that could handle this, could be:

    public void ActionResult View(string[] segments /* <- the name of the parameter must match the name of the route parameter */)
    {
        // use the segments to obtain information about the product or category and produce data to the user
        // ...
    }

## Routing basics


## Adding custom route in Mvc
User can add custom route, mapping an URL to a specific action in a controller. This is used for search engine optimization purpose and make URLs readable. 

    routes.MapRoute(
      name: "AboutUsAspx", // Route name
      url: "AboutUs.aspx",  // URL with parameters
      defaults: new { controller = "Home", action = "AboutUs", id = UrlParameter.Optional }  // Parameter defaults
    );

## Catch-all route for enabling client-side routing
It's a good practice to encode state of Single Page Application (SPA) in url:

    my-app.com/admin-spa/users/edit/id123

This allows to save and share application state.<br/>
When user puts url into browser's address bar and hits enter server must ignore client-side part of the requested url. If you serve your SPA as a rendered Razor view (result of calling controller's action) rather than a static html file, you can use a catch-all route:

    public class AdminSpaController
    {
        [Route("~/admin-spa/{clienSidePart*}")]
        ActionResult AdminSpa()
        {
            ...
        }
    }

In this case server returns just SPA, and it then initializes itself according to the route. This approach is more flexible as it does not depend on [url-rewrite][1] module.


  [1]: http://www.iis.net/learn/extensions/url-rewrite-module/creating-rewrite-rules-for-the-url-rewrite-module

## Attribute Routing in Areas
For using Attribute Routing in areas, registering areas and `[RouteArea(...)]` definitions are required.

In `RouteConfig.cs` :

    public class RouteConfig
    {
        public static void RegisterRoutes(RouteCollection routes)
        {
            routes.IgnoreRoute("{resource}.axd/{*pathInfo}");
            routes.MapMvcAttributeRoutes();
            AreaRegistration.RegisterAllAreas();
        }
    }

In a sample area controller attribute routing definition :

    [RouteArea("AreaName", AreaPrefix = "AreaName")]
    [RoutePrefix("SampleAreaController")]
    public class SampleAreaController : Controller
    {
        [Route("Index")]
        public ActionResult Index()
        {
            return View();
        }
    }

For using `Url.Action` links in Areas :

    @Url.Action("Index", "SampleAreaController", new { area = "AreaName" })



## Custom Routing
Custom routing provides specialized need of routing to handle specific incoming requests. 

In order to defining custom routes, keep in mind that the order of routes that you add to the route table is important.

    public static void RegisterRoutes(RouteCollection routes)
    {
        routes.IgnoreRoute("{resource}.axd/{*pathInfo}");
        
        // this is an advanced custom route
        // you can define custom URL with custom parameter(s) point to certain action method
        routes.MapRoute(
        "CustomEntry", // Route name
        "Custom/{entryId}", // Route pattern
        new { controller = "Custom", action = "Entry" } // Default values for defined parameters above
        );

        // this is a basic custom route
        // any custom routes take place on top before default route
        routes.MapRoute(
        "CustomRoute", // Route name
        "Custom/{controller}/{action}/{id}", // Route pattern
        new { controller = "Custom", action = "Index", id = UrlParameter.Optional } // Default values for defined parameters above
        );
    
        routes.MapRoute(
        "Default", // Route name
        "{controller}/{action}/{id}", // Route pattern
        new { controller = "Home", action = "Index", id = UrlParameter.Optional } // Default values for defined parameters above
        );
    }

`controller` and `action` names are reserved. By default MVC maps `{controller}` part of the URL to the class `<controller>Controller`, and then looks for a method with the name `<action>` without adding any suffixes.

Though it may be tempting to create a family of routes using `{controller}/{action}/{parameter}` template consider that by doing this you disclose the  structure of your application and make URLs somewhat brittle because changing the name of the controller changes the route and breaks the links saved by the user.

Prefer explicit route setting:

    routes.MapRoute(
        "CustomRoute", // Route name
        "Custom/Index/{id}", // Route pattern
        new { controller = "Custom", action = nameof(CustomController.Index), id = UrlParameter.Optional }
    );

(you cannot use `nameof` operator for controller name as it will have additional suffix `Controller`) which must be omitted when setting controller name in the route.

