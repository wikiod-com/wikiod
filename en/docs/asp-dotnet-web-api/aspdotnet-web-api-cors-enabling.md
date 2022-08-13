---
title: "ASP.NET WEB API CORS Enabling"
slug: "aspnet-web-api-cors-enabling"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Configure CORS for WebAPI 2 with Windows Authentication
The following server-side configuration allows CORS request to work along with Windows Authentication (no anonymous must be enabled in IIS).

**web.config** - allow unauthenticated (anonymous) preflight requests (OPTIONS)

    <system.web>
        <authentication mode="Windows" />
        <authorization>
            <allow verbs="OPTIONS" users="*"/>
            <deny users="?" />
        </authorization>
    </system.web>

**global.asax.cs** - properly reply with headers that allow caller from another domain to receive data

    protected void Application_AuthenticateRequest(object sender, EventArgs e)
    {
        if (Context.Request.HttpMethod == "OPTIONS")
        {
            if (Context.Request.Headers["Origin"] != null)
                Context.Response.AddHeader("Access-Control-Allow-Origin", Context.Request.Headers["Origin"]);
    
            Context.Response.AddHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, MaxDataServiceVersion");
            Context.Response.AddHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
            Context.Response.AddHeader("Access-Control-Allow-Credentials", "true");
    
            Response.End();
        }
    }

**CORS enabling**

    public static class WebApiConfig
    {
        public static void Register(HttpConfiguration config)
        {
            // all requests are enabled in this example. SupportsCredentials must be here to allow authenticated requests          
            var corsAttr = new EnableCorsAttribute("*", "*", "*") { SupportsCredentials = true };
            config.EnableCors(corsAttr);
        }
    }

    protected void Application_Start()
    {
        GlobalConfiguration.Configure(WebApiConfig.Register);
    }

## Enabling CORS for WebAPI 2
    // Global.asax.cs calls this method at application start
    public static void Register(HttpConfiguration config)
    {
        // New code
        config.EnableCors();
    }
    
    //Enabling CORS for controller after the above registration
    [EnableCors(origins: "http://example.com", headers: "*", methods: "*")]
    public class TestController : ApiController
    {
        // Controller methods not shown...
    }

## Enabling CORS globally for Web API 2
    public static void Register(HttpConfiguration config)
    {
        var corsAttr = new EnableCorsAttribute("http://example.com", "*", "*");
        config.EnableCors(corsAttr);
    }

## Enabling CORS in Asp.Net 5 for all domains and methods
    public void ConfigureServices(IServiceCollection services)
    {
        services.AddCors(o => o.AddPolicy("MyPolicy", builder =>
        {
            builder.AllowAnyOrigin()
                   .AllowAnyMethod()
                   .AllowAnyHeader();
        }));
    
        // ...
    }
    
    public void Configure(IApplicationBuilder app)
    {
        app.UseCors("MyPolicy");
    
        // ...
    }

## Enabling CORS in Asp.Net 5 for specific domains and methods
    public void ConfigureServices(IServiceCollection services)
    {
        services.AddMvc();
        services.AddCors();
        services.ConfigureCors(options =>
             options.AddPolicy("AllowSpecific", p => p.WithOrigins("http://localhost:1233")
                                                       .WithMethods("GET")
                                                       .WithHeaders("name")));
    }

## Properly send authenticated request from jQuery against Web API 2 endpoint
The following example shows how to properly construct both GET and POST requests against Web API 2 (CORS must be configured server-side, if sent from another domain):

    <script type="text/javascript" src="https://code.jquery.com/jquery-3.1.1.js"></script>
    CORS with Windows Authentication test
    <script type="text/javascript">
    
        // GET
        $.ajax({
            url: "endpoint url here",
            type: "GET",
            dataType: "json",
                xhrFields: {
                withCredentials: true
            }
        })
        .done(function (data, extra) {
          alert("GET result" + JSON.stringify(data));
        })
        .fail(function(data, extra) {
        });
        
        //POST
        $.ajax({
            url: "url here",
            type: "POST",
            contentType: 'application/json; charset=utf-8',
            data: JSON.stringify({testProp: "test value"}),
            xhrFields: {
                withCredentials: true
            },
            success: function(data) { 
                alert("POST success - " + JSON.stringify(data)); 
            }
        })
        .fail(function(data) {
            alert("Post error: " + JSON.stringify(data.data));
        });
        
    </script>

**Server-side code:**

        [System.Web.Http.HttpGet]
        [System.Web.Http.Route("GetRequestUsername")]
        public HttpResponseMessage GetRequestUsername()
        {
            var ret = Request.CreateResponse(
                HttpStatusCode.OK,
                new { Username = SecurityService.GetUsername() });
            return ret;
        }

        [System.Web.Http.HttpPost]
        [System.Web.Http.Route("TestPost")]
        public HttpResponseMessage TestPost([FromBody] object jsonData)
        {
            var ret = Request.CreateResponse(
                HttpStatusCode.OK,
                new { Username = SecurityService.GetUsername() });
            return ret;
        }

## Properly send authenticated request from AngularJS against Web API 2 endpoint
    <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/angular.js/1.6.1/angular.js"></script>
    CORS with Windows Authentication test (Angular)
    <script type="text/javascript">
    
        var app = angular.module('myApp', []);
        app.controller('myCtrl', function($http) {
        
            $http(
                {    
                    method: 'GET',
                    url: 'url here',
                    withCredentials: true,
                }
            )
            .then(function(data) {
                alert("Get result = " + JSON.stringify(data.data));
            },
            function(data, extra) {
                alert("Get failed: " + JSON.stringify(data.data));
            });
    
            $http(
                {
                    method: 'POST',
                    url: "url here", 
                    withCredentials: true,
                    data: { url: "some url", message: "some message", type: "some type"}
                }
            )
            .then(function(data) {
                alert("POST success - " + JSON.stringify(data.data));
            },
            function(data) {
                alert("POST failed: " + JSON.stringify(data.data));
            });
        });
    
    </script>
        
    <div ng-app="myApp" ng-controller="myCtrl">
    </div>
    

