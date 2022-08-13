---
title: "ActionResult"
slug: "actionresult"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
 - // ActionResult method returns an instance that derives from ActionResult. You are able to create action method that can return any instance that is wrapped in appropriate ActionResult type.

 -  //Built-in ActionResult return types are:

 - View(); // ViewResult renders a view as a WebPage
 - PartialView(); // PartialViewResult renders a partial view, which can be used as a part of another view.
 - Redirect(); // RedirectResult redirects to another action method by using its URL.
 - RediectToAction();  RedirectToRoute(); // RedirectToRouteResult redirects to another action method.
 - Content(); // ContentResult returns a user-defined content-type.
 - Json(); // JsonResult returns a serialized JSON object.
 - JavaScript(); // JavaScriptResult returns a script that can be executed on client side.
 - File(); // FileResult returns a binary output to write to the reponse.
 - // EmptResult represents a return value that is used if action method must return a null result.

## Action Methods
When user enters an URL, for example: http://example-website.com/Example/HelloWorld, MVC application will use the routing rules to parse this url and extract the subpath, that will determine the controller, action and possible parameters. For the above url, the result will be /Example/HelloWorld, which by default routing rules results provides the name of the controller: Exmaple and the name of the action: HelloWorld.

    public class ExampleController: Controller
    {
        public ActionResult HelloWorld()
        {
            ViewData["ExampleData"] = "Hello world!";
            return View();
        }
    }

The above ActionResult method "HelloWorld" will render the view called HelloWorld, where we can then use the data from ViewData.

## Mapping Action-Method Parameters
 If there would be another value in the URL like: /Example/ProcessInput/2, the routing rules will threat the last number as a parameter passed into the action ProcessInput of controller Example.

    public ActionResult ProcessInput(int number)
    {
        ViewData["OutputMessage"] = string.format("The number you entered is: {0}", number);
        return View();
    }

## Calling An ActionResult In Another ActionResult
We can call an action result in another action result.

    public ActionResult Action1()
    {
        ViewData["OutputMessage"] = "Hello World";
        return RedirectToAction("Action2","ControllerName");
        //this will go to second action;
    }


    public ActionResult Action2()
    {
        return View();
        //this will go to Action2.cshtml as default;
    }



