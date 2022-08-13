---
title: "Change default view location"
slug: "change-default-view-location"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

In ASP.NET MVC, the views are placed by default in the `Views` folder. Sometimes you want to change this locations and store the views somewhere else.

## Create a View Location Expander
To be able to change the view location, you need to implement the `IViewLocationExpander`. The `ExpandViewLocations` method returns an `IEnumerable<string>` containing the different locations where to search, with 

    public class MyViewLocationExpander : IViewLocationExpander
    {
        public IEnumerable<string> ExpandViewLocations(ViewLocationExpanderContext context, IEnumerable<string> viewLocations)
        {
            yield return "/CustomViewFolder/{1}/{0}.cshtml";
            yield return "/SharedFolder/{0}.cshtml";
        }

        public void PopulateValues(ViewLocationExpanderContext context)
        {            
        }
    }

## Register the View Location Expander
You now need to register the Expander, in order for it to be used by the Razor View Engine. Just add this in the `ConfigureServices` of your `Startup` class.

    public void ConfigureServices(IServiceCollection services)
    {
        services.Configure<RazorViewEngineOptions>(options => {
            options.ViewLocationExpanders.Add(new MyViewLocationExpander());
        });
    }

