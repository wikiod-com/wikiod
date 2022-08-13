---
title: "Web API Url Routing"
slug: "web-api-url-routing"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## How Routing works in asp.net webapi
In ASP.NET Web API, a controller is a class that handles HTTP requests. The public methods of the controller are called action methods or simply actions.

When the Web API framework receives a request, it routes the request to an action.
To determine which action to invoke, the framework uses a routing table. The Visual Studio project template for Web API creates a default route:

    routes.MapHttpRoute(
        name: "API Default",
        routeTemplate: "**api/{controller}/{id}**",
        defaults: new { id = RouteParameter.Optional }
    );
This route is defined in the WebApiConfig.cs file, which is placed in the App_Start directory:
[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/rIFvr.png
Each entry in the routing table contains a route template. The default route template for Web API is "**api/{controller}/{id}**". In this template, "**api**" is a literal path segment, and {**controller**} and {**id**} are placeholder variables.

When the Web API framework receives an HTTP request, it tries to match the URI against one of the route templates in the routing table. If no route matches, the client receives a 404 error. 

For example, the following URIs match the default route:

 - /api/values
 - /api/values/1

However, the following URI does not match, because it lacks the "**api**" segment:

 - /values/1

Once a matching route is found, Web API selects the controller and the action:

 - To find the controller, Web API adds "Controller" to the value of the
   {controller} variable.
 - To find the action, Web API looks at the HTTP method, and then looks
   for an action whose name begins with that HTTP method name. For
   example, with a GET request, Web API looks for an action that starts
   with "Get...", such as "GetEmployee" or "GetAllEmployees".  This
   convention applies only to GET, POST, PUT, and DELETE methods.

 You can enable other HTTP methods by using attributes on your controller. We’ll see an     example of that later.

 - Other placeholder variables in the route template, such as {id}, are
   mapped to action parameters.

**HTTP Methods**
Instead of using the naming convention for HTTP methods, you can explicitly specify the HTTP method for an action by decorating the action method with the HttpGet, HttpPut, HttpPost, or HttpDelete attribute.

In the following example, the EmployeeGetEmployee method is mapped to GET requests:   

     public class EmployeesController : ApiController
        {
            [HttpGet]
            public EmployeeGetEmployee(id) {}
        }   

     

To allow multiple HTTP methods for an action, or to allow HTTP methods other than GET, PUT, POST, and DELETE, use the AcceptVerbs attribute, which takes a list of HTTP methods.

    public class EmployeesController: ApiController
    {
        [AcceptVerbs("GET", "HEAD")]
        public Employee GetEmployee (id) { }
    }

**Routing by Action Name**

With the default routing template, Web API uses the HTTP method to select the action. However, you can also create a route where the action name is included in the URI:

    routes.MapHttpRoute(
        name: "ActionApi",
        routeTemplate: "api/{controller}/{action}/{id}",
        defaults: new { id = RouteParameter.Optional }
    );

In this route template, the {action} parameter names the action method on the controller. With this style of routing, use attributes to specify the allowed HTTP methods. For example, suppose your controller has the following method:

    public class EmployeesController: ApiController
    {
        [HttpGet]
        public List<Employee> GetAllEmployees();
    }

In this case, a GET request for “**api/Employees/GetAllEmployees**” would map to the GetAllEmployees method. 

You can override the action name by using the ActionName attribute. In the following example, there are two actions that map to "**api/Employees/ShowAllEmployees/id**. One supports GET and the other supports POST:

    public class EmployeesController : ApiController
    {
        [HttpGet]
        [ActionName("ShowAllEmployees")]
        public List<Employee> GetAll(int id);
    
        [HttpPost]
        [ActionName("ShowAllEmployees")]
        public void GetAll (int id);
    
    }

**Non-Actions**

We can prevent a method from getting invoked as an action by using the NonAction attribute. This signals to the framework that the method is not an action, even if it would otherwise match the routing rules.

     [NonAction]  
    public string GetValues() { ... }




## Verb based routing examples.
The same URl for different http methods acts differently. Below is a table depicting the same.


| HTTP VERB | URL| DESCRIPTION|
| ------ | ------ | ------ | 
| GET    | /api/students  | Returns all students| 
| GET    | /api/students/5| Returns details of Student Id =5| 
| POST   | /api/students  | Add a new student|
| PUT    | /api/students/5| Update student with Id=5| 
| DELETE | /api/students/5| Delete student with Id = 5| 



