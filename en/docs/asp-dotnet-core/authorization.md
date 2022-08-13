---
title: "Authorization"
slug: "authorization"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Simple Authorization
Authorization in asp.net core is simply `AuthorizeAttribute`

    [Authorize]
    public class SomeController : Controller
    {
        public IActionResult Get()
        {
        }

        public IActionResult Post()
        {
        }
    }

This will only allow a logged in user to access these actions.

or use the following to only limit a single action

    public class SomeController : Controller
    {
        public IActionResult Get()
        {
        }

        [Authorize]
        public IActionResult Post()
        {
        }
    }


If you want to allow all users to access one of the actions you can use `AllowAnonymousAttribute`

    [Authorize]
    public class SomeController: Controller
    {
        public IActionResult Get()
        {
        }
        
        [AllowAnonymous]
        public IActionResult Post()
        {
        }
    }

Now  `Post` can be accessed by any user.
`AllowAnonymous` always comes as a priority to authorize, so if a controller is set to `AllowAnonymous` then all it's actions are public, regardless of if they have an `AuthorizeAttribute` or not.

There is an option to set all controllers to require authorized requests -

    services.AddMvc(config =>
    {
        var policy = new AuthorizationPolicyBuilder()
            .RequireAuthenticatedUser()
            .Build();
        config.Filters.Add(new AuthorizeFilter(policy));
    }) 

This is done by adding a default authorization policy to each controller - any `Authorize`/`AllowAnonymous`  Attributes over a controller/action will override these settings.

