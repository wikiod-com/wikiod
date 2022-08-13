---
title: "Cross-Origin Requests (CORS)"
slug: "cross-origin-requests-cors"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

Browser security prevents a web page from making AJAX requests to another domain. This restriction is called the same-origin policy, and prevents a malicious site from reading sensitive data from another site. However, sometimes you might want to let other sites make cross-origin requests to your web app.

Cross Origin Resource Sharing (CORS) is a W3C standard that allows a server to relax the same-origin policy. Using CORS, a server can explicitly allow some cross-origin requests while rejecting others. CORS is safer and more flexible than earlier techniques such as JSONP.

## Enable CORS policy for all controllers
To enable a CORS policy across all of your MVC controllers you have to build the policy in the AddCors extension within the ConfigureServices method and then set the policy on the `CorsAuthorizationFilterFactory`

```
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Cors.Internal;
...
public void ConfigureServices(IServiceCollection services) {
    // Add AllowAll policy just like in single controller example.
    services.AddCors(options => {
        options.AddPolicy("AllowAll",
           builder => {
                builder.AllowAnyOrigin()
                       .AllowAnyMethod()
                       .AllowAnyHeader();
        });
    });

    // Add framework services.
    services.AddMvc();

    services.Configure<MvcOptions>(options => {
        options.Filters.Add(new CorsAuthorizationFilterFactory("AllowAll"));
    });
}

public void Configure(IApplicationBuilder app) {
    app.useMvc();
    // For content not managed within MVC. You may want to set the Cors middleware
    // to use the same policy.
    app.UseCors("AllowAll");
}
```

This CORS policy can be overwritten on a controller or action basis, but this can set the default for the entire application.

## Enable CORS for all requests
Use the `UseCors()` extension method on the `IApplicationBuilder` in the `Configure` method to apply the CORS policy to all requests.

    public void ConfigureServices(IServiceCollection services)
    {
        services.AddMvc();
        services.AddCors();
    }
    
    public void Configure(IApplicationBuilder app)
    {
        // Other middleware..

        app.UseCors(builder =>
        {
            builder.AllowAnyOrigin()
                   .AllowAnyHeader()
                   .AllowAnyMethod();
        });

        // Other middleware..

        app.UseMvc();
    }

## Enable CORS policy for specific controllers
To enable a certain CORS policy for specific controllers you have to build the policy in the `AddCors` extension within the `ConfigureServices` method:

    services.AddCors(cors => cors.AddPolicy("AllowAll", policy =>
    {
        policy.AllowAnyOrigin()
                .AllowAnyMethod()
                .AllowAnyHeader();
    }));

This allows you to apply the policy to a controller:

    [EnableCors("AllowAll")]
    public class HomeController : Controller
    {
       // ...
    }

## More sophisticated CORS policies
The policy builder allows you to build sophisticated policies.

    app.UseCors(builder =>
    {
        builder.WithOrigins("http://localhost:5000", "http://myproductionapp.com")
               .WithMethods("GET", "POST", "HEAD")
               .WithHeaders("accept", "content-type", "origin")
               .SetPreflightMaxAge(TimeSpan.FromDays(7));
    });

This policy only allows the origins `http://localhost:5000` and  `http://myproductionapp.com` with only the `GET`, `POST` and `HEAD` methods and only accepts the `accept`, `content-type` and `origin` HTTP headers. The `SetPreflightMaxAge` method causes the browsers to cache the result of the preflight request (`OPTIONS`) to be cached for the specified amount of time.

