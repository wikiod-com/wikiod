---
title: "Getting started with asp.net-mvc"
slug: "getting-started-with-aspnet-mvc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello MVC!
*ASP.NET MVC* is open source web application framework. MVC itself is a design pattern which is built around three main components: *model-view-controller*.

**Model** - Models reflect your business objects, and are a means to pass data between Controllers and Views.

**View** - Views are the pages that render and display the model data to the user. ASP.NET MVC Views are typically written using Razor syntax.

**Controller** - Controllers handle incoming HTTP requests from a client, and usually return one or more Models to an appropriate View.

**The ASP.NET MVC features:**

 1. Ideal for developing complex but light weight applications 
 2. It provides an extensible and pluggable framework which can be easily
    replaced and customized. For example, if you do not wish to use the
    in-built Razor or ASPX View Engine, then you can use any other
    third-party view engines or even customize the existing ones.
 3. Utilizes the component-based design of the application by logically
    dividing it into Model, View and Controller components. This enables
    the developers to manage the complexity of large-scale projects and
    work on individual components. 
 4. The MVC structure enhances the test-driven development and testability of the application since all the components can be designed interface-based and tested using mock
    objects. Hence the ASP.NET MVC Framework is ideal for projects with
    large team of web developers. 
5. Supports all the existing vast ASP.NET
    functionalities such as Authorization and Authentication, Master
    Pages, Data Binding, User Controls, Memberships, ASP.NET Routing,
    etc.
6.  It does not use the concept of View State (which is present in
    ASP.NET). This helps in building applications which are light-weight
    and gives full control to the developers.

**Simple MVC application**

We are going to create simple MVC application which displays person details. Create new MVC project using Visual Studio. Add new model named *Person* to Models folder as following:

    public class Person
    {
        public string Surname { get; set; }
        public string FirstName { get; set; }
        public string Patronymic { get; set; }
        public DateTime BirthDate { get; set; }
    }

Add new controller to Controllers folder:

    public class HomeController : Controller
    {
        //Action Method
        public ActionResult Index()
        {
            // Initialize model
            Person person = new Person
            {
                Surname = "Person_SURNAME",
                FirstName = "Person_FIRSTNAME",
                Patronymic = "Person_PATRONYMIC",
                BirthDate = new DateTime(1990, 1, 1)
            };

            // Send model to View for displaying to user
            return View(person);
        }
    }

Finally add View to */Views/Home/* folder named *Index.cshtml*:

    @* Model for this view is Person *@
    @model Hello_MVC.Models.Person
    
    <h2>Hello @Model.FirstName !</h2>
    
    <div>
        <h5>Details:</h5>
        <div>
            @Html.LabelFor(m => m.Surname)
            @Html.DisplayFor(m => m.Surname)
        </div>
        <div>
            @Html.LabelFor(m => m.FirstName)
            @Html.DisplayFor(m => m.FirstName)
        </div>
        <div>
            @Html.LabelFor(m => m.Patronymic)
            @Html.DisplayFor(m => m.Patronymic)
        </div>
        <div>
            @Html.LabelFor(m => m.BirthDate)
            @Html.DisplayFor(m => m.BirthDate)
        </div>
    </div>




