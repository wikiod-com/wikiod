---
title: "Getting started with asp.net-core-mvc"
slug: "getting-started-with-aspnet-core-mvc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Add MVC Middleware
If you created an empty project, or you still don't have mvc configured in your application, you can add dependency:

<!-- language-all: c# -->
`"Microsoft.AspNetCore.Mvc": "1.0.1"`

To your `project.json` file under `"dependencies"`.

And register MVC middleware in your Startup class:

    public void ConfigureServices(IServiceCollection services)
    {
        ...
        services.AddMvc();
    }

Note that we have both `services.AddMvc()` and `services.AddMvcCore()`. If you are starting with `asp.net core`, or you want it the way it is, you should keep with `services.AddMvc()`. But if you want an advanced experience, you can start with a minimal MVC pipeline and add features to get a customized framework using `services.AddMvcCore()`. See [this discussion][1] for more information about `AddMvcCore`

    public void ConfigureServices(IServiceCollection services)
    {
        services
            .AddMvcCore()
            .AddAuthorization()
            .AddJsonFormatters(j => j.Formatting = Formatting.Indented);
    }

Now you can tell your application builder to use the mvc:

    public void Configure(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory)
    {
        ...
        app.UseMvc();
    }

or with default routing:

    app.UseMvc(routes =>
    {
        routes.MapRoute(
            name: "default",
            template: "{controller=Home}/{action=Index}/{id?}");
    });


  [1]: https://github.com/aspnet/Mvc/issues/2872

## Installation or Setup
**Installing Visual Studio**
----------------------------

If you do not have Visual Studio installed, you can [download the free Visual Studio Community Edition here][1]. If you already have it installed, you can proceed to the next step.

**Creating an ASP.NET Core MVC Application.**
------------------------------------------------

 1. **Open Visual Studio.**
 2. **Select File > New Project.**
 3. **Select Web under the language of your choice** within the Templates section on the left.
 4. **Choose a preferred Project type** within the dialog.
 5. **Optional: Choose a .NET Framework you would like to target** 
 6. **Name your project** and indicate if you want to create a Solution for the project.
 7. **Click OK** to create the project.

[![enter image description here][2]][2]

You will be presented with another dialog to select the template you want to use for the project :

[![enter image description here][3]][3]

Each of the descriptions are self-explanatory. For this first project, **select Web Application**, which will contain all of the default configurations, authentication, and some existing content. 

Since this is an introduction application and doesn't require any security or authentication, you can **change the authentication option to No Authentication** on the right-side of the dialog and **click OK to create the project**.

You should then see the new project within the Solution Explorer :

[![enter image description here][4]][4]

**Press the F5 key to run the application** and begin a debugging session, which will launch the application within your default browser :

[![enter image description here][5]][5]

You can now see that your project is up and running locally and is ready as a starting point for you to build your application.

_PS: Used [Getting started with asp.net-core][6] topic from the [asp.net-core][7] Documentation._

  [1]: https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx
  [2]: http://i.stack.imgur.com/pMtnk.png
  [3]: http://i.stack.imgur.com/uOxqu.png
  [4]: http://i.stack.imgur.com/K0Fx5.png
  [5]: http://i.stack.imgur.com/h93dH.png
  [6]: https://www.wikiod.com/asp-dotnet-core/getting-started-with-aspnet-core
  [7]: https://www.wikiod.com/docs/asp-dotnet-core

## Dependency injection basics
Almost any controller needs some external dependencies to work.
Here is a way to configure a dependency object (or its factory) and pass it to a controller. Doing so will help to sustain a [separation of concerns][1], keep code clear and testable.
<!-- language-all: c# -->
Say, we have an interface and its implementation that needs some values from config in its constructor:

    public interface ISomeDependency
    {
        async Task<IEnumerable<string>> GetItemsAsync(string key);
    }

    public class SomeDependency : ISomeDependency
    {
        public SomeDependency(string connectionString)
        {
            ...
        }
        ...
    }

It's used in some controller class:

    public class SomeController : Controller
    {
        private reanonly ISomeDependency dependency;

        public SomeController(ISomeDependency dependency)
        {
            ...
            this.dependency = dependency;
        }

        ...

        public async Task<IEnumerable<string>> Get(string key) =>
            await dependency.GetItemsAsync(key);
    }

One can inject this dependency in the controller constructor calling `services.AddTransient` inside `Startup.ConfigureServices` method:

    public class Startup
    {
        public Startup(IHostingEnvironment env)
        {
            var builder = new ConfigurationBuilder().
                .SetBasePath(env.ContentRootPath)
                .AddJsonFile("appsettings.json", optional: true, reloadOnChange: true)
            ...
            Configuration = builder.Build();
        }

        public IConfigurationRoot Configuration { get; }

        public void ConfigureServices(IServiceCollection services)
        {
            ...
            services.AddTransient(serviceProvider =>
                new MyDependency(Configuration["Data:ConnectionString"]));
        }

        ...
    }

Here `Data:ConnectionString` is a path to a setting in `appsettings.json` file:

    {
      ...
      },
      "Data": {
        "ConnectionString": "some connection string"
      }
    }


Lifetime management
-------------------

To manage a lifetime of the injected object, along with `AddTransient` another two options exist: `AddSingleton` and `AddScoped`. The last one means that lifetime of the object is scoped to a HTTP request.


  [1]: https://en.wikipedia.org/wiki/Separation_of_concerns

## Versions
[Official roadmap @ Github][1]

| Version | Announcements      | Release Date      |
| ------- | ------------------ | ------------------|
| RC1*    | [1.0.0-rc1][2]     | 2015-11-01        |
| RC2*    | [1.0.0-rc2][3]     | 2016-05-16        |
| 1.0.0   | [1.0.0][4]         | 2016-06-27        |
| 1.0.1   | [1.0.1][5]         | 2016-09-13        |
| 1.0.1   | [1.0.1][5]         | 2016-09-13        |
| 1.1     | [1.1.0][6]         | Q4 2016 / Q1 2017 |
| 1.2     | [1.2.0][7]         | Q1 2017 / Q2 2017 |


_* References to yearly quarters (Q1, Q2, Q3, Q4) are calendar-based_

  [1]: https://github.com/aspnet/Home/wiki/Roadmap
  [2]: https://github.com/aspnet/Announcements/milestone/6
  [3]: https://github.com/aspnet/Announcements/milestone/7
  [4]: https://github.com/aspnet/Announcements/milestone/8
  [5]: https://github.com/aspnet/Announcements/milestone/10
  [6]: https://github.com/aspnet/Announcements/issues?utf8=%E2%9C%93&q=is%3Aopen%20is%3Aissue%20milestone%3A1.1.0
  [7]: https://github.com/aspnet/Announcements/issues?utf8=%E2%9C%93&q=is%3Aopen%20is%3Aissue%20milestone%3A1.2.0%20

