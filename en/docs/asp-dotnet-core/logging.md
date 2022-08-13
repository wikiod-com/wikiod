---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

## Add Logger to Controller
Instead of  requesting an ILoggerFactory and creating an instance of ILogger explicitly, you can request an ILogger<T> (where T is the class requesting the logger).

    public class TodoController : Controller
    {
        private readonly ILogger _logger;
    
        public TodoController(ILogger<TodoController> logger)
        {
         _logger = logger;
        }
    }

## Using NLog Logger
[NLog.Extensions.Logging][1] is the official [NLog][2] provider for Microsoft's in .NET Core and ASP.NET Core. [Here][3] and [here][4] are instruction and example respectively.


  [1]: https://github.com/NLog/NLog.Extensions.Logging
  [2]: http://nlog-project.org
  [3]: https://github.com/NLog/NLog.Extensions.Logging/blob/master/README.md
  [4]: https://github.com/NLog/NLog.Extensions.Logging/tree/master/examples/aspnet-core-example

## Using Serilog in ASP.NET core 1.0 application
1)In project.json, add below dependencies-

    "Serilog": "2.2.0",
    "Serilog.Extensions.Logging": "1.2.0",
    "Serilog.Sinks.RollingFile": "2.0.0",
    "Serilog.Sinks.File": "3.0.0"

2)In Startup.cs, add below lines in constructor-

    Log.Logger = new LoggerConfiguration()
        .MinimumLevel.Debug()
        .WriteTo.RollingFile(Path.Combine(env.ContentRootPath, "Serilog-{Date}.txt"))
        .CreateLogger();

3)In Configure method of Startup class-

    loggerFactory.AddSerilog();

4)In Controller, create instance of ILogger like this-

    public class HomeController : Controller
    {
        ILogger<HomeController> _logger = null;
        public HomeController(ILogger<HomeController> logger)
        {
            _logger = logger;
        }

5)Sample logging below-

    try
    {
        throw new Exception("Serilog Testing");
    }
    catch (System.Exception ex)
    {
        this._logger.LogError(ex.Message);
    }

