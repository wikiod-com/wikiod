---
title: "T4MVC"
slug: "t4mvc"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

[T4MVC][1] is a [T4 template][0] that generates strongly-typed helpers for use in MVC Routing mechanisms, as opposed to magic strings. T4MVC will detect the various controllers, actions, and views, and create references to those views, making compile-time errors in the event that an attempt to Route or access a View is invalid.


[0]: https://msdn.microsoft.com/en-us/library/bb126445.aspx?f=255&MSPPError=-2147217396\
[1]: https://github.com/T4MVC/T4MVC

## Calling an Action
In MVC, there are some scenerios where you want to specify an action for routing purposes, either for a link, form action, or a redirect to action. You can specify an action via the MVC namespace.

When given a Controller, such as `HomeController`:

    public class HomeController : Controller
    {
        public ActionResult Index()
        {
            ...
        }
        public ActionResult MyAction()
        {
            ...
        }
        public ActionResult MyActionWithParameter(int parameter)
        {
            ...
        }
    }

T4MVC will generate an inherited controller that overrides the action. This override will set the route data properly so that MVC's `UrlHelper` will generate the proper Url. You can call this method and pass it into the various methods for `UrlHelper`. The examples below assume the default MVC route is being used:

**Link**

To generate an `a` tag with the specified text:

    @Html.ActionLink("Link Text", MVC.Home.Index() )
    //result: <a href="/">Link Text</a>
    @Html.ActionLink("Link Text", MVC.Home.MyAction() )
    //result: <a href="/Home/MyAction">Link Text</a>
    //T4MVC also allows you to specify the parameter without creating an anonymous object:
    @Html.ActionLink("Link Text", MVC.Home.MyActionWithParameter(1) )
    //result: <a href="/Home/MyActionWithParameter/1">Link Text</a>

To generate a url:

    @Url.Action( MVC.Home.Index() )
    //result: /
    @Url.Action("Link Text", MVC.Home.MyAction() )
    //result: /Home/MyAction
    @Url.Action("Link Text", MVC.Home.MyActionWithParameter(1) )
    //result: /Home/MyActionWithParameter/1

Notice that T4MVC follows the same rules as MVC Routing - it won't specify default route variables, so that the `Index` action on the `HomeController` doesn't generate `/Home/Index` but instead the perfectly valid and abbreviated form of `/`.

**Form Method**

To generate a `form` tag with the correct `action` specified:

    @Html.BeginForm( MVC.Home.Index(), FormMethod.Get /* or FormMethod.Post */ )
    {
        //my form
    }
    //result:
    <form action="/" method="GET">
        //my form
    </form>
    @Html.BeginForm( MVC.Home.MyActionWithParameter(1), FormMethod.Get /* or FormMethod.Post */ )
    {
        //my form
    }
    //result:
    <form action="/Home/MyActionWithParameter/1" method="GET">
        //my form
    </form>

**Redirect To Action**

When in a controller, you may want to redirect to an action from the current action. This can be done, likeso:

    public class RedirectingController : Controller
    {
        public ActionResult MyRedirectAction()
        {
            ...
            return RedirectToAction( MVC.Redirecting.ActionToRedirectTo() );
            //redirects the user to the action below.
        }
        public ActionResult ActionToRedirectTo()
        {
            ...
        }
    }







