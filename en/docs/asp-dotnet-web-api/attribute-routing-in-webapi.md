---
title: "Attribute Routing in WebAPI"
slug: "attribute-routing-in-webapi"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

As the name suggests, this uses attributes to route. This gives the user more control over the URI's in the WebAPI. For example, you can describe hierarchies of the resource. However, the earlier 'Conventional Routing' is fully supported. Users can have a mixture of both too.

## Syntax
 - [RoutePrefix("api/books")]  - for controller class   
 - [Route("getById")]          - for actions
 - [Route("~/api/authors/{authorId:int}/books")]  - for overriding route prefix

  


## Parameters
| Parameter Name | Details |
| ------ | ------ |
| RoutePrefix   | attribute to the controller class. all common url prefixes in actions are clubbed here. takes string as input    |
| Route   |  attribute to the controller actions. each action will have route assosciated with(not necessarily)    |
| Route("~/api/")   |  this overrides the Route Prefix   |

Currently, Attribute Routes doesn't have `Controller specific Message Handlers`. As there is no way to specify Which handler to execute for which route at the time of declaration. This is possible in `Conventional Routing`. 

## Basic Attribute Routing
Simply add an attribute to the controller action

    [Route("product/{productId}/customer")]
    public IQueryable<Product> GetProductsByCustomer(int productId) 
    { 
        //action code goes here 
    }

this will be queried as `/product/1/customer` and `productId=1` will be sent to the controller action.     

Make sure the one within '{ }' and the action parameter are same. `productId` in this case. 

before using this, you have to specify that you are using Attribute Routing by:
    
    public static class WebApiConfig
    {
        public static void Register(HttpConfiguration config)
        {
            config.MapHttpAttributeRoutes();
        }
    }

## Route Prefix Attribute
In cases where you need a common portion of the route for all routes within a controller, [`RoutePrefix`][1] attribute is used.

In the below example, api/students part of the code is common and so we can define `RoutePrefix` and avoid using it repeatedly.

    [RoutePrefix("api/students")]
    public class StudentController : ApiController
    {
    [Route("")]
    public IEnumerable<Student> Get() 
    {
        //action code goes here 
    }
 
    [Route("{id:int}")]
    public Student Get(int id) 
    {
        //action code goes here 
    }

    [Route("")]
    public HttpResponseMessage Post(Student student) 
    {
        //action code goes here 
    }

    }


  [1]: https://msdn.microsoft.com/en-us/library/system.web.http.routeprefixattribute(v=vs.118).aspx

