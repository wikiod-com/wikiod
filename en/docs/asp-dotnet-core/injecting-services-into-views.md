---
title: "Injecting services into views"
slug: "injecting-services-into-views"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
* `@inject<NameOfService><Identifier>`
* `@<Identifier>.Foo()`
* @inject \<type\> \<name\>

## The @inject Directive
ASP.NET Core introduces the concept of dependency injection into Views via the `@inject` directive via the following syntax : 

    @inject <type> <name>

**Example Usage**
---

Adding this directive into your View will basically generate a property of the given type using the given name within your View using proper dependency injection as demonstrated in the example below :

    @inject YourWidgetServiceClass WidgetService

    <!-- This would call the service, which is already populated and output the results -->
    There are <b>@WidgetService.GetWidgetCount()</b> Widgets here.

**Required Configuration**
---

Services that use dependency injection are still required to be registered within the `ConfigureServices()` method of the `Startup.cs` file and scoped accordingly :


    public void ConfigureServices(IServiceCollection services)
    {
         // Other stuff omitted for brevity 

         services.AddTransient<IWidgetService, WidgetService>();
    }
    

