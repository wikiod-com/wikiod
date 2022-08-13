---
title: "Sessions in ASP.NET Core 1.0"
slug: "sessions-in-aspnet-core-10"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Using Sessions in ASP.NET Core 1.0

## Basic example of handling Session


1)First, add dependency in project.json - `"Microsoft.AspNetCore.Session": "1.1.0",`

2)In `startup.cs` and add `AddSession()` and `AddDistributedMemoryCache()` lines to the `ConfigureServices` like this-

    services.AddDistributedMemoryCache(); //This way ASP.NET Core will use a Memory Cache to store session variables
    services.AddSession(options =>
            {
                options.IdleTimeout = TimeSpan.FromDays(1); // It depends on user requirements.
                options.CookieName = ".My.Session"; // Give a cookie name for session which will be visible in request payloads.
            });

3)Add the `UseSession()` call in Configure method of startup like this-

    app.UseSession(); //make sure add this line before UseMvc()

4)In Controller, Session object can be used like this-

    using Microsoft.AspNetCore.Http;
    
    public class HomeController : Controller
    {
        public IActionResult Index()
        { 
            HttpContext.Session.SetString("SessionVariable1", "Testing123");
            return View();
        }
    
        public IActionResult About()
        {
            ViewBag.Message = HttpContext.Session.GetString("SessionVariable1");
    
            return View();
        }
    }

 5) If you are using cors policy then sometimes it may give errors, after enabling  
    session regarding headers about enabling *AllowCredentials* header and using  
    *WithOrigins* header instead of *AllowAllOrigins*.

