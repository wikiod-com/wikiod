---
title: "Automatic client-side validation from attributes"
slug: "automatic-client-side-validation-from-attributes"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

By default, Safari does not enforce HTML5 element validation. You need to override this manually using other means.

## Model
```
public class UserModel 
{

        [Required]
        [StringLength(6, MinimumLength = 3)]      
        [RegularExpression(@"(\S)+", ErrorMessage = "White space is not allowed")]       
        public string UserName { get; set; }

        [Required]
        [StringLength(8, MinimumLength = 3)]        
        public string FirstName { get; set; }
        
        [Required]
        [StringLength(9, MinimumLength = 2)]        
        public string LastName { get; set; }

        [Required]
        public string City { get; set; }

}
```

## web.config settings
```
<appSettings>
  <add key="ClientValidationEnabled" value="true"/> 
  <add key="UnobtrusiveJavaScriptEnabled" value="true"/> 
 </appSettings>
```

## Required Nuget Packages
```
<package id="jQuery" version="1.10.2" targetFramework="net452" />
<package id="jQuery.Validation" version="1.11.1" targetFramework="net452" />
<package id="Microsoft.jQuery.Unobtrusive.Validation" version="3.2.3" targetFramework="net452" />
```

## Form View
```
@model WebApplication4.Models.UserModel
@{
    ViewBag.Title = "Register";
}

<h2>@ViewBag.Title.</h2>

@using (Html.BeginForm("Register", "Account", FormMethod.Post, new { @class = "form-horizontal", role = "form" }))
{
    @Html.AntiForgeryToken()
    <h4>Create a new account.</h4>
    <hr />
    @Html.ValidationSummary("", new { @class = "text-danger" })
    <div class="form-group">
        @Html.LabelFor(m => m.FirstName, new { @class = "col-md-2 control-label" })
        <div class="col-md-10">
            @Html.TextBoxFor(m => m.FirstName, new { @class = "form-control" })
            @Html.ValidationMessageFor(m=>m.FirstName)
        </div>
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.LastName, new { @class = "col-md-2 control-label" })
        <div class="col-md-10">
            @Html.TextBoxFor(m => m.LastName, new { @class = "form-control" })
            @Html.ValidationMessageFor(m => m.LastName)
        </div>
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.UserName, new { @class = "col-md-2 control-label" })
        <div class="col-md-10">
            @Html.TextBoxFor(m => m.UserName, new { @class = "form-control" })
            @Html.ValidationMessageFor(m => m.UserName)
        </div>
    </div>
    <div class="form-group">
        <div class="col-md-offset-2 col-md-10">
            <input type="submit" class="btn btn-default" value="Register" />
        </div>
    </div>
}

@section Scripts {
    @Scripts.Render("~/bundles/jqueryval")
}

```

## Bundle configuration
```
  public class BundleConfig
    {
       
        public static void RegisterBundles(BundleCollection bundles)
        {
            bundles.Add(new ScriptBundle("~/bundles/jquery").Include(
                        "~/Scripts/jquery-{version}.js"));

            bundles.Add(new ScriptBundle("~/bundles/jqueryval").Include(
                        "~/Scripts/jquery.validate*"));

          
        }
    }


## Global.asax.cs
```
public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            AreaRegistration.RegisterAllAreas();
            FilterConfig.RegisterGlobalFilters(GlobalFilters.Filters);
            RouteConfig.RegisterRoutes(RouteTable.Routes);

            // Need to include your bundles
            BundleConfig.RegisterBundles(BundleTable.Bundles);
        }
    }
```

