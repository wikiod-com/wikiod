---
title: "Katana"
slug: "katana"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

**What Is Katana?**

Katana is a set of open source components for building and hosting OWIN-based web applications, maintained by the Microsoft Open Technologies Group.Katana provides an implementation of the OWIN specification, and is in fact used in an increasing number of ASP.NET project templates. Additionally, Katana provides a wide variety of ready-to-use middleware components, ready for use in an OWIN-based application.

   
    
  

## Example
**Basic KatanaConsole Application**

    namespace KatanaConsole
    {
        // use an alias for the OWIN AppFunc:
        using AppFunc = Func<IDictionary<string, object>, Task>;
     
        class Program
        {
            static void Main(string[] args)
            {
                WebApp.Start<Startup>("http://localhost:8080");
                Console.WriteLine("Server Started; Press enter to Quit");
                Console.ReadLine();
            }
        }
     
        public class Startup
        {
            public void Configuration(IAppBuilder app)
            {
                var middleware = new Func<AppFunc, AppFunc>(MyMiddleWare);
                app.Use(middleware);
            }
     
            public AppFunc MyMiddleWare(AppFunc next)
            {
                AppFunc appFunc = async (IDictionary<string, object> environment) =>
                {
                    // Do something with the incoming request:
                    var response = environment["owin.ResponseBody"] as Stream;
                    using (var writer = new StreamWriter(response))
                    {
                        await writer.WriteAsync("<h1>Hello from My First Middleware</h1>");
                    }
                    // Call the next Middleware in the chain:
                    await next.Invoke(environment);
                };
                return appFunc;
            }
        }
    }

