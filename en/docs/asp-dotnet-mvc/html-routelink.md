---
title: "Html.RouteLink"
slug: "htmlroutelink"
draft: false
images: []
weight: 9889
type: docs
toc: true
---

## Parameters
| Parameter| Details |
| ------ | ------ |
| linkText   | The text that will be displayed for the link.   |
| routeName   | The name of the route to return a virtual path for.   |

## Basic Example Using Link Text and Route Name
As an alternative to using [`Html.ActionLink`][1] to generate links in a view, you can use

[`Html.RouteLink`][2]

To make use of this feature, you need to configure a route, for example:

    public static void RegisterRoutes(RouteCollection routes)
    {
      routes.MapRoute(
        "SearchResults",
        "{controller}/{action}",
        new { controller = "Search", action = "Results" });
    }

Then in a view you can create a link to that route like so:

`@Html.RouteLink("Search Results", "SearchResults");`

Using `RouteLink()` is convenient if you end up changing controller names, or action method names, since using `Html.ActionLink()` means having to change the controller and action method name parameters in the call, so that they match the new names which have been changed.

With `RouteLink()` you can change the route details in the `MapRoute()` call, in other words in one location, and any code that is referencing that route via `RouteLink()` will not be required to change.


  [1]: https://msdn.microsoft.com/en-us/library/system.web.mvc.html.linkextensions.actionlink(v=vs.118).aspx
  [2]: https://msdn.microsoft.com/en-us/library/system.web.mvc.html.linkextensions.routelink(v=vs.118).aspx


