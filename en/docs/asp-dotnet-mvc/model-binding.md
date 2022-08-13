---
title: "Model binding"
slug: "model-binding"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

Model binding is the process of taking HTTP parameters, typically in the Query String of a GET request, or within POST body, and applying it into an object that can then be validated and consumed in an object-oriented manner without the need for Controller actions having intimate knowledge of how to retrieve HTTP parameters.

In other words, model binding is what allows actions, in MVC, to have either parameter(s), whether it being a value type or an object.

To try to create instance in the action, the bind model process will search data in various places: 

 - Form Data 
 - Route Data 
 - Query String 
 - Files Custom (cookies for example)

## File Upload
**Model:**

    public class SampleViewModel
    {
        public HttpPostedFileBase file {get;set;}
    }

**View:**

    @model HelloWorldMvcApp.SampleViewModel

    @using (Html.BeginForm("Index","Home",FormMethod.Post, new { enctype = "multipart/form-data" }))
    {
        <div class="form-group">
            @Html.TextBoxFor(model => model.file, new {@class="form-control", type="file"}) 
            @Html.ValidationMessageFor(model => model.file)
        </div>
    
        <button type="submit" class="btn btn-success submit">Upload</button>
    }

**Action:**

    [HttpPost]
    public ActionResult Index(SampleViewModel model)
    {                
        
        if (model.file.ContentLength > 0) 
        {
            string fileName = Path.GetFileName(model.file.FileName);
            string fileLocation = "~/App_Data/uploads/"+ fileName;
            model.file.SaveAs(Server.MapPath(fileLocation));
        }
        return View(model);
    }

## Prevent binding on PostModel
Considering a (post)model:

    public class User
    {
        public string FirstName { get; set; }
        public bool IsAdmin { get; set; }
    }

With a view like so:

    @using (Html.BeginForm()) {
        @Html.EditorFor(model => model.FirstName)
        <input type="submit" value="Save" />       
    }

In order to prevent a malicious user from assigning IsAdmin you can use the `Bind` attribute in the action:

    [HttpPost]
    public ViewResult Edit([Bind(Exclude = "IsAdmin")] User user)
    {
        // ...
    }


## Route value binding
Given some default routing such as `{controller=Home}/{action=Index}/{id?}`
 if you had the url `https://stackoverflow.com/questions/1558902`

This would go to the QuestionsController and the value 1558902 would be mapped to an id parameter of an index action, i.e.

    public ActionResult Index(int? id){
         //id would be bound to id of the route
    }

## Query string binding
To extend on the route binding say you had a url like `https://stackoverflow.com/questions/1558902?sort=desc`

and routing like `{controller=Home}/{action=Index}/{id?}`

    public ActionResult Index(int? id, string sort){
         //sort would bind to the value in the query string, i.e. "desc"
    }



## Binding to objects
Often you'd be working with viewmodel classes in asp.net-mvc and would want to bind to properties on these.  This works similar to mapping to individual parameters.

Say you had a simple view model call PostViewModel like this

    public class PostViewModel{
      public int Id {get;set;}
      public int SnappyTitle {get;set;}
    }

Then you had posted values of Id and SnappyTitle from a form in the http request then they would map right onto that model if the model itself was the action parameter, e.g.

    public ActionResult UpdatePost(PostViewModel viewModel){
      //viewModel.Id would have our posted value
    }
It's worth noting the binding is case insensitive for the parameter and property names.  It will also cast values where possible.  I'm leaving more edge cases for specific examples

## Ajax binding
These are form values that go in the HTTP request using the POST method. (including jQuery POST requests).

Say you did an ajax post like

    $.ajax({
        type: 'POST',
        url: window.updatePost,
        data:  { id: 21, title: 'snappy title' },
        //kept short for clarity
    });

Here the two values in json, id and title, would be bound to the matching action, e.g.

    public JsonResult UpdatePost(int id, string title) {
        ...
    }

## Generic, Session based model binding
Sometimes we need preserve *whole model* and transfer it across actions or even controllers. Storing model at session good solution for this type of requirements. If we combine this with powerful *model binding* features of MVC we get elegant way of doing so. We can create generic session based model binding in three easy steps:

**Step one: Create model binder**

Create a model binder itself. Personally I created *SessionDataModelBinder* class in */Infrastructure/ModelBinders* folder.

    using System;
    using System.Web.Mvc;
    
    public class SessionDataModelBinder<TModel>
        : IModelBinder
        where TModel : class
    {
        private string SessionKey { get; set; }
    
        public SessionDataModelBinder(string sessionKey)
        {
            if (string.IsNullOrEmpty(sessionKey))
                throw new ArgumentNullException(nameof(sessionKey));
            SessionKey = sessionKey;
        }
    
        public object BindModel(
            ControllerContext controllerContext, 
            ModelBindingContext bindingContext)
        {
            // Get model from session
            TModel model = controllerContext
                .HttpContext
                .Session[SessionKey] as TModel;
            // Create model if it wasn't found from session and store it
            if (model == null)
            {
                model = Activator.CreateInstance<TModel>();
                controllerContext.HttpContext.Session[SessionKey] = model;
            }
            // Return the model
            return model;
        }
    }

**Step two: register binder**

If we have model like below:

    public class ReportInfo
    {
        public int ReportId { get; set; }
        public ReportTypes TypeId { get; set; }
    }

    public enum ReportTypes
    {
        NotSpecified,
        Monthly, Yearly
    }
    
We can register session based model binder for this model in **Global.asax** in *Application_Start* method:

    protected void Application_Start()
    {
        .........

        // Model binders.
        // Remember to specy unique SessionKey
        ModelBinders.Binders.Add(typeof(ReportInfo), 
            new SessionDataModelBinder<ReportInfo>("ReportInfo"));
    }
    
**Step three: use it!**

Now we can benefit from this model binder simply by *adding parameter to our actions*:

    public class HomeController : Controller
    {
        public ActionResult Index(ReportInfo reportInfo)
        {
            // Simply set properties
            reportInfo.TypeId = ReportTypes.Monthly;

            return View();
        }

        public ActionResult About(ReportInfo reportInfo)
        {
            // reportInfo.TypeId is Monthly now because we set
            // it previously in Index action.
            ReportTypes currentReportType = reportInfo.TypeId;

            return View();
        }
    }

## Validating date fields manually with dynamic formats using model binder
If different users need different datetime format then you may need to parse your incoming date string to actual date according to the format. In this case this snippet may help you. 
              
    public class DateTimeBinder : DefaultModelBinder 
    {
        public override object BindModel(ControllerContext controllerContext, ModelBindingContext bindingContext)
        {

                    var value = bindingContext.ValueProvider.GetValue(bindingContext.ModelName);
                    DateTime date;
                    var displayFormat = Session["DateTimeFormat"];
                    if (value.AttemptedValue != "")
                    {
                        if (DateTime.TryParseExact(value.AttemptedValue, displayFormat, CultureInfo.InvariantCulture, DateTimeStyles.None, out date))
                        {
                            return date;
                        }
                        else
                        {
                            bindingContext.ModelState.AddModelError(bindingContext.ModelName, "Invalid date format");
                        }
                    }
                }

            return base.BindModel(controllerContext, bindingContext);
        }

