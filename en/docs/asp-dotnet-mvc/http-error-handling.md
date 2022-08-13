---
title: "Http Error Handling"
slug: "http-error-handling"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Every website needs to handle errors. You could let your users see the stock 404 or 500 error pages that IIS dishes out or, using the Web.Config and a simple Controller you can capture these errors and deliver your own custom error pages. 


## Basic Setup
This example will cover creating a custom error page for 404 Page Not Found and 500 Server Error. You can extend this code to capture any error code you need to. 


**Web.Config**

If you are using IIS7 and above, ignore the `<CustomError..` node and use `<httpErrors...` instead. 

Add in the following in the `system.webServer` node: 

    <httpErrors errorMode="Custom" existingResponse="Replace">
        <remove statusCode="404" />
        <remove statusCode="500" />
        <error statusCode="404" path="/error/notfound" responseMode="ExecuteURL" />
        <error statusCode="500" path="/error/servererror" responseMode="ExecuteURL" />
     </httpErrors>

This tells the site to direct any 404 errors to `~/error/notfound` and any 500 error to `~/error/servererror`. It will also preserve your requested URL (think _transfer_ rather than _redirect_) so the user will never see the `~/error/...` page URL. 

Next, you need a new `Error` controller so...

    public class ErrorController : Controller
    {
        public ActionResult servererror()
        {
            Response.TrySkipIisCustomErrors = true;
            Response.StatusCode = (int)HttpStatusCode.InternalServerError;
            return View();
        }

        public ActionResult notfound()
        {
            Response.TrySkipIisCustomErrors = true;
            Response.StatusCode = (int)HttpStatusCode.NotFound;
            return View();
        }

    } 

The key thing to note here is the `Response.TrySkipIisCustomErrors = true;`. This will bypass IIS and force your error page through. 

Lastly, create the corresponding `NotFound` and `ServerError` Views and style them up so it's all nice and seamless with your site's design.

Hey presto - custom error pages. 

