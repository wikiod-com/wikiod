---
title: "Partial Views"
slug: "partial-views"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

A partial view is a view that is rendered within another view. Partial views can be reused and thus prevent duplication of code. They can be rendered by Html.Partial or Html.RenderPartial

## Syntax
 - @Html.Partial("ViewName") 
   
    @Html.Partial("ViewName",ViewModel)
   
    @{Html.RenderPartial("ViewName");}
   
   If your partial view is located in a different folder other than
   shared folder, then you will have to mention full path of the view as
   below:
   
   -@Html.RenderPartial("~/Areas/Admin/Views/Shared/partial/_subcat.cshtml")

## Partial View with model


## Partial View to a String - for email content etc
Calling the function
 

    string InvoiceHtml = myFunction.RenderPartialViewToString("PartialInvoiceCustomer", ToInvoice); // ToInvoice is a model, you can pass parameters if needed

Function to generate HTML

    public static string RenderPartialViewToString(string viewName, object model)
    {
        using (var sw = new StringWriter())
        {
            BuyOnlineController controller = new BuyOnlineController(); // instance of the required controller (you can pass this as a argument if needed)

            // Create an MVC Controller Context
            var wrapper = new HttpContextWrapper(System.Web.HttpContext.Current);

            RouteData routeData = new RouteData();

            routeData.Values.Add("controller", controller.GetType().Name.ToLower().Replace("controller", ""));

            controller.ControllerContext = new ControllerContext(wrapper, routeData, controller);

            controller.ViewData.Model = model;

            var viewResult = ViewEngines.Engines.FindPartialView(controller.ControllerContext, viewName);

            var viewContext = new ViewContext(controller.ControllerContext, viewResult.View, controller.ViewData, controller.TempData, sw);
            viewResult.View.Render(viewContext, sw);

            return sw.ToString();
        }
    }

Partial View - PartialInvoiceCustomer

    @model eDurar.Models.BuyOnlineCartMaster
     <h2>hello customer - @Model.CartID </h2>



## Html.Partial Vs Html.RenderPartial
**Html.Partial** returns a string on the other hand **Html.RenderPartial** returns void.
 
**Html.RenderPartial**

  This method returns void and the result is directly written to the HTTP response stream. That means it uses the same TextWriter object used in the current webpage/template.
 For this reason, this method is faster than Partial method.This method is useful when the displaying data in the partial view is already in the corresponding view model.

**Example :** In a blog to show comments of an article, we would like to use RenderPartial method since an article information with comments are already populated in the view model.

    @{Html.RenderPartial("_Comments");}

**Html.Partial**

This method returns an HTML-encoded string. This can be stored in a variable. Like RenderPartial method, Partial method is also useful when the displaying data in the partial view is already in the corresponding view model. 

**Example:** In a blog to show comments of an article, you can use Partial method since an article information with comments are already populated in the view model.

    @Html.Partial("_Comments")


