---
title: ".net core support status"
slug: "net-core-support-status"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This topic shows the latest status of NancyFx compatibility with .Net Core. Community support is highly welcome to update this. 



## Status
> Currently there are numerous incompatibilities with .Net core. This will show status of NancyFx package status. This is grouped by functionality. Currently support is for Nancy 2.* which has not released a stable version yet. 

# Nancy core 
Nancy : 2.0.0-clinteastwood

# Nancy Boostrappers
Nancy.Bootstrappers.Ninject - Not Supported 

# Nancy View Engines
Nancy.ViewEngines.Razor - Not Supported
[issue #2521](https://github.com/NancyFx/Nancy/issues/2521)

# Nancy Authentication

Nancy.Authentication.Stateless : 2.0.0-clinteastwood

# Nancy FluentValidation
Note: Must use at least package ""FluentValidation.AspNetCore : 6.4.0-beta9

Nancy.Validation.FluentValidation : 2.0.0-clinteastwood

## Routing changes
 No Longer supported:

    Get ["/"] = parameters => {
        return View["index"];
    };
Changed to:

    Get("/", parameters => {
        return View["index"];
    });



