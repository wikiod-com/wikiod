---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Redirect to custom error page
ASP.NET Core provides the [status code pages middleware][1], that supports several different extension methods, but we are interesting in `UseStatusCodePages` and `UseStatusCodePagesWithRedirects`:

- [UseStatusCodePages][2] adds a StatusCodePages middleware with the given options that checks for responses with status codes between 400 and 599 that do not have a body. Example of use for redirect:

      app.UseStatusCodePages(async context => {
        //context.HttpContext.Response.StatusCode contains the status code

        // your redirect logic

      });


- [UseStatusCodePagesWithRedirects][2] adds a StatusCodePages middleware to the pipeline. Specifies that responses should be handled by redirecting with the given location URL template. This may include a '{0}' placeholder for the status code. URLs starting  with '~' will have PathBase prepended, where any other URL will be used as is. 
For example the following will redirect to ~/errors/<error_code> (for example ~/errors/403 for 403 error):

      app.UseStatusCodePagesWithRedirects("~/errors/{0}");


  [1]: https://docs.asp.net/en/latest/fundamentals/error-handling.html#configuring-status-code-pages
  [2]: https://github.com/aspnet/Diagnostics/blob/48b436ec8a2aa2d8ebcfc8682e85d336e217c8ce/src/Microsoft.AspNetCore.Diagnostics/StatusCodePage/StatusCodePagesExtensions.cs

## Global Exception Handling in ASP.NET Core
UseExceptionHandler can be used to handle exceptions globally. You can get all the details of exception object like Stack Trace, Inner exception and others. And then you can show them on screen. You can easily implement like this. 

    app.UseExceptionHandler(
     options => {
        options.Run(
        async context =>
        {
          context.Response.StatusCode = (int)HttpStatusCode.InternalServerError;
          context.Response.ContentType = "text/html";
          var ex = context.Features.Get<IExceptionHandlerFeature>();
          if (ex != null)
          {
            var err = $"<h1>Error: {ex.Error.Message}</h1>{ex.Error.StackTrace }";
            await context.Response.WriteAsync(err).ConfigureAwait(false);
          }
        });
     }
    );

You need to put this inside configure() of startup.cs file. 

