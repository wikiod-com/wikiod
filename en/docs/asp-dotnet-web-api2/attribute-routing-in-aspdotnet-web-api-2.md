---
title: "Attribute Routing in ASP.NET Web API 2"
slug: "attribute-routing-in-aspnet-web-api-2"
draft: false
images: []
weight: 9993
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

Currently, Attribute Routes doesn't have Controller specific Message Handlers. As there is no way to specify Which handler to execute for which route at the time of declaration. This is possible in Conventional Routing.

## Basic Attribute Routing

Simply add an attribute to the controller action

    [Route("product/{productId}/customer")]
    public IQueryable<Product> GetProductsByCustomer(int productId) 
    { 
        //action code goes here 
    }
this will be queried as /product/1/customer and productId=1 will be sent to the controller action.

Make sure the one within '{ }' and the action parameter are same. productId in this case.

before using this, you have to specify that you are using Attribute Routing by:

    public static class WebApiConfig
    {
        public static void Register(HttpConfiguration config)
        {
            config.MapHttpAttributeRoutes();
        }
    }

## Route prefixes
Usually, the routes in a controller have the same prefix connected somehow with functionality of this controller. For example:

    public class ProductsController : ApiController
    {
        [Route("api/products")]
        public IEnumerable<Product> GetProducts() { ... }
    
        [Route("api/products/{id:int}")]
        public Product GetProduct(int id) { ... }
    
        [Route("api/products")]
        [HttpPost]
        public HttpResponseMessage CreateProduct(Product product) { ... }
    }

In such scenario we can set common prefix for whole controller. To do so we use the `[RoutePrefix]` attribute:


    [RoutePrefix("api/products")]
    public class ProductsController : ApiController
    {
        // GET api/products
        [Route("")]
        public IEnumerable<Product> GetProducts() { ... }
    
        // GET api/products/5
        [Route("{id:int}")]
        public Product GetProduct(int id) { ... }
    
        //POST api/products
        [Route("")]
        [HttpPost]
        public HttpResponseMessage CreateProduct(Product product) { ... }
    }

**Overriding route prefix**

If we want to override the route prefix we can use a tilde **(~)** in the routing attribute of the method:

    [RoutePrefix("api/products")]
    public class ProductsController : ApiController
    {
        // GET api/owners/products
        [Route("~/api/owners/products")]
        public IEnumerable<Product> GetProducts() { ... }
    
        //...
    }

