---
title: "Html.AntiForgeryToken"
slug: "htmlantiforgerytoken"
draft: false
images: []
weight: 9844
type: docs
toc: true
---

The anti-forgery token can be used to help protect your application against cross-site request forgery. To use this feature, call the AntiForgeryToken method from a form and add the ValidateAntiForgeryTokenAttribute attribute to the action method that you want to protect.

Generates a hidden form field (anti-forgery token) that is validated when the form is submitted.

## Syntax
- @Html.AntiForgeryToken()

When submitting an ajax request with CSRF token (`__RequestVerificationToken`) make sure that content type is not set to `application/json`. If you are using jQuery it automatically sets the content type to `application/x-www-form-urlencoded` which is then recognised by ASP.NET MVC. 

Caution
-------

Use caution when setting this value. Using it improperly can open security vulnerabilities in the application.

## Basic usage
The `@Html.AntiForgeryToken()` helper method protects against [cross-site request forgery](https://www.owasp.org/index.php/Top_10_2013-A8-Cross-Site_Request_Forgery_(CSRF)) (or CSRF) attacks.

It can be used by simply using the `Html.AntiForgeryToken()` helper  within one of your existing forms and decorating its corresponding Controller Action with the `[ValidateAntiForgeryToken]` attribute.

**Razor (YourView.cshtml)**
----

    @using (Html.BeginForm("Manage", "Account")) {
        @Html.AntiForgeryToken()  
        <!-- ... -->
    }

OR

    <form>
        @Html.AntiForgeryToken()
        <!-- ... -->
    </form>

**Controller (YourController.cs)**
----

The target action method:

    [ValidateAntiForgeryToken]
    [HttpPost]
    public ActionResult ActionMethod(ModelObject model)
    {
        // ...
    }

## Disable Identity Heuristic Check
Often times you will see an exception

    Anti forgery token is meant for user "" but the current user is "username"

This is because the Anti-Forgery token is also linked to the current logged-in user. This error appears when a user logs in but their token is still linked to being an anonymous user for the site.

There are a few ways to fix this behavior, but if you would rather not have CSRF tokens linked to the logged-in state of a user you may disable this feature.

Put this line in your `Global.asax` file or similar application startup logic.

    AntiForgeryConfig.SuppressIdentityHeuristicChecks = true;

## Validating All Posts
Due to the vulnerability caused by CSRF, it is generally considered a good practice to check for an AntiForgeryToken on all HttpPosts unless there is a good reason to not do it (some technical issue with the post, there is another authentication mechanism and/or the post does not mutate state like saving to a db or file). To ensure that you don't forget, you can add a special GlobalActionFilter that automatically checks all HttpPosts unless the action is decorated with a special "ignore" attribute. 

    [AttributeUsage(AttributeTargets.Class)]
    public class ValidateAntiForgeryTokenOnAllPosts : AuthorizeAttribute
    {
        public override void OnAuthorization(AuthorizationContext filterContext)
        {
            var request = filterContext.HttpContext.Request;

            //  Only validate POSTs
            if (request.HttpMethod == WebRequestMethods.Http.Post)
            {
                bool skipCheck = filterContext.ActionDescriptor.IsDefined(typeof(DontCheckForAntiForgeryTokenAttribute), true)
                    || filterContext.ActionDescriptor.ControllerDescriptor.IsDefined(typeof(DontCheckForAntiForgeryTokenAttribute), true);

                if (skipCheck)
                    return;


                //  Ajax POSTs and normal form posts have to be treated differently when it comes
                //  to validating the AntiForgeryToken
                if (request.IsAjaxRequest())
                {
                    var antiForgeryCookie = request.Cookies[AntiForgeryConfig.CookieName];

                    var cookieValue = antiForgeryCookie != null
                        ? antiForgeryCookie.Value
                        : null;

                    AntiForgery.Validate(cookieValue, request.Headers["__RequestVerificationToken"]);
                }
                else
                {
                    new ValidateAntiForgeryTokenAttribute()
                        .OnAuthorization(filterContext);
                }
            }
        }
    }

    /// <summary>
    /// this should ONLY be used on POSTS that DO NOT MUTATE STATE
    /// </summary>
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Method, AllowMultiple = false, Inherited = true)]
    public sealed class DontCheckForAntiForgeryTokenAttribute : Attribute { }

To make sure it gets checked on all requests, just add it to your Global Action Filters

    public class FilterConfig
    {
        public static void RegisterGlobalFilters(GlobalFilterCollection filters)
        {
            //...
            filters.Add(new ValidateAntiForgeryTokenOnAllPosts());
            //...
        }
    }

## Using AntiForgeryToken with Jquery Ajax Request
First off you create the form

    @using (Html.BeginForm())
    {
      @Html.AntiForgeryToken()
    }
Action Method

    [HttpPost]
    [ValidateAntiForgeryToken]
    public ActionResult Test(FormViewModel formData)
    {
        // ...
    }

Script

    <script src="https://code.jquery.com/jquery-1.12.4.min.js"></script>
    <script>
    var formData = new FormData($('form')[0]);
    $.ajax({
        method: "POST",
        url: "/demo/test",
        data: formData ,
        success: function (data) {
      console.log(data);
        },
        error: function (jqXHR, textStatus, errorThrown) {
            console.log(errorThrown);
        }
    })
    </script>
Make sure contentType isn't set to anything apart from `application/x-www-form-urlencoded` and if its not specified Jquery defaults to `application/x-www-form-urlencoded` 

## Advance usage: Apply default Antiforgery filter for every POST
We may forget to apply the `Antiforgery attribute` for each `POST` request so we should make it by default. This sample will make sure `Antiforgery filter` will always be applied to every `POST` request.

Firstly create new `AntiForgeryTokenFilter` filter:

    //This will add ValidateAntiForgeryToken Attribute to all HttpPost action methods
    public class AntiForgeryTokenFilter : IFilterProvider
    {
        public IEnumerable<Filter> GetFilters(ControllerContext controllerContext, ActionDescriptor actionDescriptor)
        {
            List<Filter> result = new List<Filter>();

            string incomingVerb = controllerContext.HttpContext.Request.HttpMethod;

            if (String.Equals(incomingVerb, "POST", StringComparison.OrdinalIgnoreCase))
            {
                result.Add(new Filter(new ValidateAntiForgeryTokenAttribute(), FilterScope.Global, null));
            }

            return result;
        }
    }

Then register this custom filter to MVC, Application_Start:

    public class FilterConfig
    {
        public static void RegisterGlobalFilters(GlobalFilterCollection filters)
        {    
            //Cactch generic error
            filters.Add(new HandleErrorAttribute());

            //Anti forgery token hack for every post request
            FilterProviders.Providers.Add(new AntiForgeryTokenFilter());            
        }
    }  

 

    public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            AreaRegistration.RegisterAllAreas();
            FilterConfig.RegisterGlobalFilters(GlobalFilters.Filters);
            RouteConfig.RegisterRoutes(RouteTable.Routes);
            BundleConfig.RegisterBundles(BundleTable.Bundles);
        }
    }

So now all your `POST` requests are protected by default using Antiforgery attributes so we are no longer need to have `[ValidateAntiForgeryToken]` attribute on each POST method.

