---
title: "Middleware"
slug: "middleware"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
|`IDictionary<string,object> environment`  | This is the only collection in which OWIN communicates information during a call.   All keys can be found at https://docs.asp.net/en/latest/fundamentals/owin.html#owin-keys|


  

The `AppFunc` type is just an alias for `Func<IDictionary<string, object>, Task>` type to shorten method signatures, much like `typedef` in C++.

## Output the request path and the time it took to process it
    
    
    //define a short alias to avoid chubby method signatures
    using AppFunc = Func<IDictionary<string, object>, Task>;

    class RequestTimeMiddleware
    {
        private AppFunc _next;

        public RequestTimeMiddleware(AppFunc next)
        {
            _next = next;
        }

        public async Task Invoke(IDictionary<string, object> environment)
        {
            IOwinContext context = new OwinContext(environment);
            
            var path = context.Request.Path;
            var sw = Stopwatch.StartNew();
            //Queue up the next middleware in the pipeline
            await _next(environment);
            //When the request comes back, log the elapsed time
            Console.WriteLine($"Request for {path} processed in {sw.ElapsedMilliseconds}ms");
        }
    }

    public static class RequestTimeMiddlewareExtensions
    {
        //Extension method as syntactic sugar, to get a meaningful way 
        //in adding the middleware to the pipeline
        public static void UseRequestTimeMiddleware(this IAppBuilder app)
        {
            app.Use<RequestTimeMiddleware>();
        }
    }


    public class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            //add the Middleware as early as possible
            app.UseRequestTimeMiddleware();
            //Queue up every other module
            app.Use(async (environment, next) =>
            {
                await environment.Response.WriteAsync("Hello from the console world");
                await next();
            });
        }
    }

