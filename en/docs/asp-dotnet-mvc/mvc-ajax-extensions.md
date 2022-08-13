---
title: "MVC Ajax Extensions"
slug: "mvc-ajax-extensions"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

This documents the use of the `System.Web.Mvc.Ajax` library. 

Citing MSDN docs "Each extension method renders an HTML element. The ActionLink method renders an anchor (a) element that links to an action method. The RouteLink method renders an anchor (a) element that links to a URL, which can resolve to an action method, a file, a folder, or some other resource. This class also contains BeginForm and BeginRouteForm methods that help you create HTML forms that are supported by AJAX functions.

## Parameters

| AJAX Options  |  Description |
| ---- | --------- |
|Confirm | Gets or sets the message to display in a confirmation window before a request is submitted. |
| HttpMethod   | Gets or sets the HTTP request method ("Get" or "Post").
|InsertionMode  |  Gets or sets the mode that specifies how to insert the response into the target DOM element.
| LoadingElementDuration  |  Gets or sets a value, in milliseconds, that controls the duration of the animation when showing or hiding the loading element.
| LoadingElementId  |  Gets or sets the id attribute of an HTML element that is displayed while the Ajax function is loading.
| OnBegin  |  Gets or sets the name of the JavaScript function to call immediately before the page is updated.
| OnComplete  |  Gets or sets the JavaScript function to call when response data has been instantiated but before the page is updated.
| OnFailure  |  Gets or sets the JavaScript function to call if the page update fails.
| OnSuccess  |  Gets or sets the JavaScript function to call after the page is successfully updated.
| UpdateTargetId  |  Gets or sets the ID of the DOM element to update by using the response from the server.
|  Url  |  Gets or sets the URL to make the request to.

The package `Jquery.Unobtrusive-Ajax` is required in the project.  The corresponding javascript files must be included in a bundle (`jquery.unobtrusive-ajax.js` or `jquery.unobtrusive-ajax.min.js`). Finally, it must be activated as well in the `web.config` file:

    <appSettings>
        <add key="UnobtrusiveJavaScriptEnabled" value="true" />
    </appSettings>


The Actions invoked (`SomeAction` in the examples) must either return a `Json` or a `PartialView`. 

## Ajax Action Link
    @* Renders an anchor (a) element that links to an action method. 
     * The innerHTML of "target-element" is replaced by the result of SomeAction. 
     *@
    @Ajax.ActionLink("Update", "SomeAction", new AjaxOptions{UpdateTargetId="target-element" })



## Ajax Forms
    @* Adds AJAX functions support to a form. 
     * The innerHTML of "target-element" is replaced by the result of SomeAction. 
     *@
    @using ( Ajax.BeginForm("SomeAction", "SomeController",
                            new AjaxOptions {
                                UpdateTargetId="target-element",
                                OnSuccess = "some_js_fun(context)"
                            })
    )
    {
        <!-– my form contents -->
    }

