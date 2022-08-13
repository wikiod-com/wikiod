---
title: "Quick Start Working with JSON"
slug: "quick-start-working-with-json"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Examples to get you up and running quickly (and correctly) with ASP.NET WebAPI

## Return JSON from GET using attributes
# 1.  Setup your formatter and routing in `Register` of (`App_Start/WebApiConfig`)

    public static class WebApiConfig
    {
        public static void Register(HttpConfiguration config)
        {
            GlobalConfiguration.Configuration.Formatters.Clear();
            GlobalConfiguration.Configuration.Formatters.Add(new JsonMediaTypeFormatter());

            config.MapHttpAttributeRoutes();
        }
    }



# 2.  Create methods in an `ApiController`

    public class HelloWorldController : ApiController
    {
      [HttpGet]
      [Route("echo/{message}")]
      public IHttpActionResult Echo(string message) {
        return Ok(new{ hello: message });  
      }

      [HttpGet]
      [Route("echo/{digits:int}")]
      public IHttpActionResult Echo(int digits) {
        return Ok(new{ hello: digits });  

    }



executing `GET /echo/foo`

    {
      "hello": "foo"
    }



executing `GET /echo/1241290805`

    {
      "hello": 1241290805
    }

as the routing framework takes the most specific conditions (data type) when choosing a method

