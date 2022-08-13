---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Basic Routing
```csharp
app.UseMvc(routes => 
{ 
    routes.MapRoute( 
    name: "default", 
    template: "{controller=Home}/{action=Index}/{id?}"); 
}); 
```

This will match requests for `/Home/Index`, `/Home/Index/123` and `/`



## Routing constraints
It is possible to create custom routing constraint which can be used inside routes to constraint a parameter to specific values or pattern. 

This constrain will match a typical culture/locale pattern, like en-US, de-DE, zh-CHT, zh-Hant. 

<!-- language-all: c# -->

    public class LocaleConstraint : IRouteConstraint
    {
        private static readonly Regex LocalePattern = new Regex(@"^[a-z]{2}(-[a-z]{2,4})?$",
                                        RegexOptions.Compiled | RegexOptions.IgnoreCase);

        public bool Match(HttpContext httpContext, IRouter route, string routeKey,
                            RouteValueDictionary values, RouteDirection routeDirection)
        {
            if (!values.ContainsKey(routeKey))
                return false;

            string locale = values[routeKey] as string;
            if (string.IsNullOrWhiteSpace(locale))
                return false;

            return LocalePattern.IsMatch(locale);
        }
    }

Afterwards, the Constraint needs to be registered before it can be used in routes.

    services.Configure<RouteOptions>(options =>
    {
        options.ConstraintMap.Add("locale", typeof(LocaleConstraint));
    });

Now it can be used within routes. 

# Using it on Controllers

    [Route("api/{culture:locale}/[controller]")]
    public class ProductController : Controller { }

# Using it on Actions

    [HttpGet("api/{culture:locale}/[controller]/{productId}"]
    public Task<IActionResult> GetProductAsync(string productId) { }

# Using it in Default Routes

    app.UseMvc(routes => 
    { 
        routes.MapRoute( 
            name: "default", 
            template: "api/{culture:locale}/{controller}/{id?}"); 
        routes.MapRoute( 
            name: "default", 
            template: "api/{controller}/{id?}"); 
    });


