---
title: "Middleware"
slug: "middleware"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

Middleware is a software component that will determine how to process the request and decide whether to pass it to the next component in the application pipeline. Each middleware has a vary specific role and actions to preform on the request.

## Using the ExceptionHandler middleware to send custom JSON error to Client
<!-- language-all: c# -->
 Define your class that will represent your custom error.

    public class ErrorDto
    {
       public int Code { get; set; }
       public string Message { get; set; }

       // other fields

       public override string ToString()
       {
          return JsonConvert.SerializeObject(this);
       }
    }

Then put next [ExceptionHandler][1] middleware to Configure method. Pay attention that middleware order is important.

    app.UseExceptionHandler(errorApp =>
        {
            errorApp.Run(async context =>
            {
                context.Response.StatusCode = 500; // or another Status 
                context.Response.ContentType = "application/json";
    
                var error = context.Features.Get<IExceptionHandlerFeature>();
                if (error != null)
                {
                    var ex = error.Error;
    
                    await context.Response.WriteAsync(new ErrorDto()
                    {
                        Code = <your custom code based on Exception Type>,
                        Message = ex.Message // or your custom message
                        
                        ... // other custom data
                    }.ToString(), Encoding.UTF8);
                }
            });
        });

  [1]: https://github.com/aspnet/Diagnostics/tree/dev/src/Microsoft.AspNetCore.Diagnostics/ExceptionHandler

## Pass data through the middleware chain
<!-- language-all: c# -->
From [documentation][1]:
> The ***HttpContext.Items*** collection is the best location to store data that is only needed while processing a given request. Its contents are discarded after each request. It is best used as a means of communicating between components or middleware that operate at different points in time during a request, and have no direct relationship with one another through which to pass parameters or return values. 

`HttpContext.Items` is a simple dictionary collection of type `IDictionary<object, object>`. This collection is
-  available from the start of an `HttpRequest` 
- and is discarded at the end of each request.

You can access it by simply assigning a value to a keyed entry, or by requesting the value for a given key.

For example, some simple Middleware could add something to the Items collection:

    app.Use(async (context, next) =>
    {
        // perform some verification
        context.Items["isVerified"] = true;
        await next.Invoke();
    });

and later in the pipeline, another piece of middleware could access it:

    app.Run(async (context) =>
    {
        await context.Response.WriteAsync("Verified request? " + context.Items["isVerified"]);
    });

  [1]: https://docs.asp.net/en/latest/fundamentals/app-state.html#httpcontext-items

## Middleware to set response ContentType
<!-- language-all: c# -->
The idea is to use `HttpContext.Response.OnStarting` callback, as this is the last event that is fired before the headers are sent. Add the following to your middleware `Invoke` method.

    public async Task Invoke(HttpContext context)
    {
        context.Response.OnStarting((state) =>
        {
            if (context.Response.StatusCode == (int)HttpStatusCode.OK)
            {
               if (context.Request.Path.Value.EndsWith(".map"))
               {
                 context.Response.ContentType = "application/json";
               }
            }          
            return Task.FromResult(0);
        }, null);
    
        await nextMiddleware.Invoke(context);
    }

## Run, Map, Use
<!-- language-all: c# -->
**Run**

Terminates chain. No other middleware method will run after this. Should be placed at the end of any pipeline.

    app.Run(async context =>
    {
        await context.Response.WriteAsync("Hello from " + _environment);
    });

**Use**

Performs action before and after next delegate.

    app.Use(async (context, next) =>
    {
        //action before next delegate
        await next.Invoke(); //call next middleware
        //action after called middleware
    });

Ilustration of how it works:
[![enter image description here][1]][1]

**MapWhen**

Enables branching pipeline. Runs specified middleware if condition is met.

    private static void HandleBranch(IApplicationBuilder app)
    {
        app.Run(async context =>
        {
            await context.Response.WriteAsync("Condition is fulfilled");
        });
    }
    
    public void ConfigureMapWhen(IApplicationBuilder app)
    {
        app.MapWhen(context => {
            return context.Request.Query.ContainsKey("somekey");
        }, HandleBranch);
    }

**Map**

Similar to MapWhen. Runs middleware if path requested by user equals path provided in parameter.

    private static void HandleMapTest(IApplicationBuilder app)
    {
        app.Run(async context =>
        {
            await context.Response.WriteAsync("Map Test Successful");
        });
    }
    
    public void ConfigureMapping(IApplicationBuilder app)
    {
        app.Map("/maptest", HandleMapTest);
    
    }

Based on [ASP.net Core Docs][2]


  [1]: http://i.stack.imgur.com/YXaaj.png
  [2]: https://docs.asp.net/en/latest/fundamentals/middleware.html#middleware

