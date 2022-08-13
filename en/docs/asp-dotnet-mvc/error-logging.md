---
title: "Error Logging"
slug: "error-logging"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Simple Attribute
    using System;
    using System.Web;
    using System.Web.Mvc;
    
    namespace Example.SDK.Filters
    {
        [AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
        public sealed class CustomErrorHandlerFilter : HandleErrorAttribute
        {
            public override void OnException(ExceptionContext filterContext)
            {
                // RouteDate is useful for retrieving info like controller, action or other route values
                string controllerName = filterContext.RouteData.Values["controller"].ToString();
                string actionName = filterContext.RouteData.Values["action"].ToString();
    
                string exception = filterContext.Exception.ToString(); // Full exception stack
                string message = filterContext.Exception.Message; // Message given by the exception
    
                // Log the exception within database
                LogExtensions.Insert(exception.ToString(), message, controllerName + "." + actionName);
    
                base.OnException(filterContext);
            }
        }
    }

Then set it in `FilterConfig.cs`

    filters.Add(new CustomErrorHandlerFilter());




## returning custom error page
    public ActionResult Details( string product)
    {
       ....
        if (productNotFound) {
            // http://www.eidias.com/blog/2014/7/2/mvc-custom-error-pages
            Response.Clear();
            Response.TrySkipIisCustomErrors = true;
            Response.Write(product + " product not exists");
            Response.StatusCode = (int)HttpStatusCode.NotFound;
            Response.End();
            return null;
        }
   }

## Create Custom ErrorLogger In ASP.Net MVC


> Step 1: Creating Custom Error Logging Filter which will write Errors in Text Files According to DateWise.

    public class ErrorLogger : HandleErrorAttribute
    {
        public override void OnException(ExceptionContext filterContext)
        {

            string strLogText = "";
            Exception ex = filterContext.Exception;
            filterContext.ExceptionHandled = true;
            var objClass = filterContext;
            strLogText += "Message ---\n{0}" + ex.Message;

            if (ex.Source == ".Net SqlClient Data Provider")
            {
                strLogText += Environment.NewLine + "SqlClient Error ---\n{0}" + "Check Sql Error";
            }
            else if (ex.Source == "System.Web.Mvc")
            {
                strLogText += Environment.NewLine + ".Net Error ---\n{0}" + "Check MVC Code For Error";
            }
            else if (filterContext.HttpContext.Request.IsAjaxRequest() == true)
            {
                strLogText += Environment.NewLine + ".Net Error ---\n{0}" + "Check MVC Ajax Code For Error";
            }
            strLogText += Environment.NewLine + "Source ---\n{0}" + ex.Source;
            strLogText += Environment.NewLine + "StackTrace ---\n{0}" + ex.StackTrace;
            strLogText += Environment.NewLine + "TargetSite ---\n{0}" + ex.TargetSite;
            if (ex.InnerException != null)
            {
                strLogText += Environment.NewLine + "Inner Exception is {0}" + ex.InnerException;//error prone
            }
            if (ex.HelpLink != null)
            {
                strLogText += Environment.NewLine + "HelpLink ---\n{0}" + ex.HelpLink;//error prone
            }

            StreamWriter log;

            string timestamp = DateTime.Now.ToString("d-MMMM-yyyy", new CultureInfo("en-GB"));

            string error_folder = ConfigurationManager.AppSettings["ErrorLogPath"].ToString();

            if (!System.IO.Directory.Exists(error_folder))
            {
                System.IO.Directory.CreateDirectory(error_folder);
            }

            if (!File.Exists(String.Format(@"{0}\Log_{1}.txt", error_folder, timestamp)))
            {
                log = new StreamWriter(String.Format(@"{0}\Log_{1}.txt", error_folder, timestamp));
            }
            else
            {
                log = File.AppendText(String.Format(@"{0}\Log_{1}.txt", error_folder, timestamp));
            }

            var controllerName = (string)filterContext.RouteData.Values["controller"];
            var actionName = (string)filterContext.RouteData.Values["action"];

            // Write to the file:
            log.WriteLine(Environment.NewLine + DateTime.Now);
            log.WriteLine("------------------------------------------------------------------------------------------------");
            log.WriteLine("Controller Name :- " + controllerName);
            log.WriteLine("Action Method Name :- " + actionName);
            log.WriteLine("------------------------------------------------------------------------------------------------");
            log.WriteLine(objClass);
            log.WriteLine(strLogText);
            log.WriteLine();

            // Close the stream:
            log.Close();
            filterContext.HttpContext.Session.Abandon();
            filterContext.Result = new RedirectToRouteResult
             (new RouteValueDictionary 
             {
                     {"controller", "Errorview"}, {"action", "Error"}
             });

        }

    }


> Step 2: Adding Physical Path on Server or Local drive where text file will be stored 

    <add key="ErrorLogPath" value="C:\ErrorLog\DemoMVC\" />
    

> Step 3: Adding **Errorview** Controller with **Error** ActionMethod

> Step 4: Adding **Error.cshtml View and Display Custom Error Message on View** 

> Step 5: Register ErrorLogger Filter in FilterConfig class

    public class FilterConfig
    {
        public static void RegisterGlobalFilters(GlobalFilterCollection filters)
        {
            filters.Add(new ErrorLogger());
        }
    }

> Step 6: Register FilterConfig in Global.asax

[![enter image description here][1]][1]
 


  [1]: https://i.stack.imgur.com/E5Ilw.jpg

