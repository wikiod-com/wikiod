---
title: "View Components"
slug: "view-components"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Create a View Component
View components encapsulate reusable pieces of logic and views. They are defined by:
- A ViewComponent class containing the logic for fetching and preparing the data for the view and deciding which view to render.
- One or more views

Since they contain logic, they are more flexible than partial views while still promoting a good separation of concerns.

A simple custom view component is defined as:

    public class MyCustomViewComponent : ViewComponent
    {        
        public async Task<IViewComponentResult> InvokeAsync(string param1, int param2)
        {
            //some business logic

            //renders ~/Views/Shared/Components/MyCustom/Default.cshtml
            return View(new MyCustomModel{ ... });
        }
    }

    @*View file located in ~/Views/Shared/Components/MyCustom/Default.cshtml*@
    @model WebApplication1.Models.MyCustomModel
    <p>Hello @Model.UserName!</p>

They can be invoked from any view (or even a controller by returning a `ViewComponentResult`)
 
    @await Component.InvokeAsync("MyCustom", new {param1 = "foo", param2 = 42})




## Login View Component
The default project template creates a partial view **_LoginPartial.cshtml** which contains a bit of logic for finding out whether the user is logged in or not and find out its user name.

Since a view component might be a better fit (as there is logic involved and even 2 services injected) the following example shows how to convert the LoginPartial into a view component.

View Component class

    public class LoginViewComponent : ViewComponent
    {
        private readonly SignInManager<ApplicationUser> signInManager;
        private readonly UserManager<ApplicationUser> userManager;

        public LoginViewComponent(SignInManager<ApplicationUser> signInManager, UserManager<ApplicationUser> userManager)
        {
            this.signInManager = signInManager;
            this.userManager = userManager;
        }

        public async Task<IViewComponentResult> InvokeAsync()
        {
            if (signInManager.IsSignedIn(this.User as ClaimsPrincipal))
            {                
                return View("SignedIn", await userManager.GetUserAsync(this.User as ClaimsPrincipal));
            }
            return View("SignedOut");
        }
    }

SignedIn view (in ~/Views/Shared/Components/Login/SignedIn.cshtml)

    @model WebApplication1.Models.ApplicationUser
    
    <form asp-area="" asp-controller="Account" asp-action="LogOff" method="post" id="logoutForm" class="navbar-right">
        <ul class="nav navbar-nav navbar-right">
            <li>
                <a asp-area="" asp-controller="Manage" asp-action="Index" title="Manage">Hello @Model.UserName!</a>
            </li>
            <li>
                <button type="submit" class="btn btn-link navbar-btn navbar-link">Log off</button>
            </li>
        </ul>
    </form>

SignedOut view (in ~/Views/Shared/Components/Login/SignedOut.cshtml)

    <ul class="nav navbar-nav navbar-right">
        <li><a asp-area="" asp-controller="Account" asp-action="Register">Register</a></li>
        <li><a asp-area="" asp-controller="Account" asp-action="Login">Log in</a></li>
    </ul>

Invocation from **_Layout.cshtml**

    @await Component.InvokeAsync("Login")



## Return from Controller Action
When inheriting from base `Controller` class provided by the framework, you can use the convenience method `ViewComponent()` to return a view component from the action:

    public IActionResult GetMyComponent()
    {
        return ViewComponent("Login", new { param1 = "foo", param2 = 42 });
    }

If using a POCO class as a controller, you can manually create an instance of the `ViewComponentResult` class. This would be equivalent to the code above:

    public IActionResult GetMyComponent()
    {
        return new ViewComponentResult 
        { 
            ViewComponentName = "Login",
            Arguments = new { param1 = "foo", param2 = 42 } 
        };
    }

