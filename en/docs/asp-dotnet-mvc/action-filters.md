---
title: "Action filters"
slug: "action-filters"
draft: false
images: []
weight: 9919
type: docs
toc: true
---

## Session Control action filter - page&ajax request
Usually authentication&authorization processes are performed by built-in cookie and token supports in .net MVC. But if you decide to do it yourself with `Session` you can use below logic for both page requests and ajax requests.

    public class SessionControl : ActionFilterAttribute
    {
        public override void OnActionExecuting ( ActionExecutingContext filterContext )
        {
            var session = filterContext.HttpContext.Session;

            /// user is logged in (the "loggedIn" should be set in Login action upon a successful login request)
            if ( session["loggedIn"] != null && (bool)session["loggedIn"] )
                return;

            /// if the request is ajax then we return a json object
            if ( filterContext.HttpContext.Request.IsAjaxRequest() )
            {
                filterContext.Result = new JsonResult
                {
                    Data = "UnauthorizedAccess",
                    JsonRequestBehavior = JsonRequestBehavior.AllowGet
                };
            }
            /// otherwise we redirect the user to the login page
            else
            {
                var redirectTarget = new RouteValueDictionary { { "Controller", "Login" }, { "Action", "Index" } };
                filterContext.Result = new RedirectToRouteResult(redirectTarget);
            }
        }

        public override void OnResultExecuting ( ResultExecutingContext filterContext )
        {
            base.OnResultExecuting(filterContext);
            
            /// we set a field 'IsAjaxRequest' in ViewBag according to the actual request type
            filterContext.Controller.ViewBag.IsAjaxRequest = filterContext.HttpContext.Request.IsAjaxRequest();
        }
    }

## A logging action filter
     public class LogActionFilter : ActionFilterAttribute
     {
          public override void OnActionExecuting(ActionExecutingContext filterContext)
          {
               Log("OnActionExecuting", filterContext.RouteData);       
          }

          public override void OnActionExecuted(ActionExecutedContext filterContext)
          {
               Log("OnActionExecuted", filterContext.RouteData);       
          }

          public override void OnResultExecuting(ResultExecutingContext filterContext)
          {
               Log("OnResultExecuting", filterContext.RouteData);       
          }

          public override void OnResultExecuted(ResultExecutedContext filterContext)
          {
               Log("OnResultExecuted", filterContext.RouteData);       
          }


          private void Log(string methodName, RouteData routeData)
          {
               var controllerName = routeData.Values["controller"];
               var actionName = routeData.Values["action"];
               var message = String.Format("{0} controller:{1} action:{2}", methodName, controllerName, actionName);
               Debug.WriteLine(message, "Action Filter Log");
          }
     }

## Action filter usage locations (global, controller, action)
You can place action filters at three possible levels:

 1. Global
 2. Controller
 3. Action

Placing a filter **globally** means it will execute on requests to any route. Placing one on a **controller** makes it execute on requests to any action in that controller. Placing one on an **action** means it runs with the action.

If we have this simple action filter:

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Method, AllowMultiple = true)]
    public class CustomActionFilterAttribute : FilterAttribute, IActionFilter
    {
        private readonly string _location;

        public CustomActionFilterAttribute(string location)
        {
            _location = location;
        }

        public void OnActionExecuting(ActionExecutingContext filterContext)
        {
            Trace.TraceInformation("OnActionExecuting: " + _location);
        }

        public void OnActionExecuted(ActionExecutedContext filterContext)
        {
            Trace.TraceInformation("OnActionExecuted: " + _location);
        }
    }

We can add it on global level by adding it to the global filter collection. With the typical ASP.NET MVC project setup, this is done in App_Start/FilterConfig.cs.

    public class FilterConfig
    {
        public static void RegisterGlobalFilters(GlobalFilterCollection filters)
        {
            filters.Add(new CustomActionFilterAttribute("Global"));
        }
    }

We can also add it on controller and action level like so in a controller:

    [CustomActionFilter("HomeController")]
    public class HomeController : Controller
    {
        [CustomActionFilter("Index")]
        public ActionResult Index()
        {
            return View();
        }
    }

If we run the application and look at the Output window, we will see the following messages:

    iisexpress.exe Information: 0 : OnActionExecuting: Global
    iisexpress.exe Information: 0 : OnActionExecuting: HomeController
    iisexpress.exe Information: 0 : OnActionExecuting: Index
    iisexpress.exe Information: 0 : OnActionExecuted: Index
    iisexpress.exe Information: 0 : OnActionExecuted: HomeController
    iisexpress.exe Information: 0 : OnActionExecuted: Global

As you can see, when the request comes in, the filters are executed:

 1. Global
 2. Controller
 3. Action

Excellent examples of filters placed on global level include:

 1. Authentication filters
 2. Authorization filters
 3. Logging filters

## Exception Handler Attribute

This attribute handles all unhandled exceptions in the code, (this is mostly for Ajax Requests - that deal with JSON - but can be extended)

    public class ExceptionHandlerAttribute : HandleErrorAttribute
    {
        /// <summary>
        ///   Overriden method to handle exception
        /// </summary>
        /// <param name="filterContext"> </param>
        public override void OnException(ExceptionContext filterContext)
        {
            // If exeption is handled - return ( don't do anything)
            if (filterContext.ExceptionHandled)
                return;

            // Set the ExceptionHandled to true ( as you are handling it here)
            filterContext.ExceptionHandled = true;

            //TODO: You can Log exception to database or Log File

            //Set your result structure 
            filterContext.Result = new JsonResult
            {
                Data = new { Success = false, Message = filterContext .Exception.Message, data = new {} },
                JsonRequestBehavior = JsonRequestBehavior.AllowGet
            };

        }
    }
    


So let's say you always have to send a JSON response similar to this:

    { 
    
        Success: true,  // False when Error
        
        data: {},
        
        Message:"Success" // Error Message when Error
    
    }


So instead of handling exceptions in controller actions, like this:


    public ActionResult PerformMyAction()
    {
        try
        {
            var myData = new { myValue = 1};
            
            throw new Exception("Handled", new Exception("This is an Handled Exception"));
            
            return Json(new {Success = true, data = myData, Message = ""});
        
        }
        catch(Exception ex)
        {
            return Json(new {Success = false, data = null, Message = ex.Message});
        }
    }


You can do this:

    [ExceptionHandler]
    public ActionResult PerformMyAction()
    {
            var myData = new { myValue = 1};
            
            throw new Exception("Unhandled", new Exception("This is an unhandled Exception"));
            
            return Json(new {Success = true, data = myData, Message = ""});
    }
    
OR you can add at Controller level

    [ExceptionHandler]
    public class MyTestController : Controller
    {
        
        public ActionResult PerformMyAction()
        {
                var myData = new { myValue = 1};
                
                throw new Exception("Unhandled", new Exception("This is an unhandled Exception"));
                
                return Json(new {Success = true, data = myData, Message = ""});
        }
    }
    
    


