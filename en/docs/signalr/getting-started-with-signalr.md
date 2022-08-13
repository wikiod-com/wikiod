---
title: "Getting started with signalr"
slug: "getting-started-with-signalr"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting up and running
**IIS / .NET version Requirements**: [See here](http://www.asp.net/signalr/overview/getting-started/supported-platforms)

**SignalR 2+**
-

 1. Install the NuGet package `Microsoft.AspNet.SignalR` (this is the whole SignalR solution) and it will ask you to install any dependencies for other packages. Accept the terms and install them as well.

 2. Now that we've got the `.dlls` and client scripts needed to generate our own SignalR Hub, let's create one. Click on your Web project, add a folder named `Hubs` or `SignalR`, and in it add a class <code>**NameOfYourChoosing***Hub*</code>. I will name mine `MessagingHub.cs`.

 3. We need to derive from the base class `Hub` that is in our SignalR dll that we downloaded via NuGet. The code would look like :

    <!-- language: lang-cs -->

        [HubName("messagingHub")]
        public class MessagingHub : Hub
        {
            //empty hub class
        }

    And in our Startup.cs class we can let the `IAppBuilder` know that we are going to use SignalR.

    <!-- language: lang-cs -->

        public partial class Startup
        {
            public void Configuration(IAppBuilder app)
            {
                app.MapSignalR();
                ConfigureAuth(app);
            }
        }

 4. In order to reference our hub code on the Client, we will need to import/reference 2 scripts (Aside from the obvious jQuery reference). The main `jQuery.signalR-version.js` file and the generated `hubs.js` file that SignalR generates for our hub specifically. These resources may look like this: 

    <!-- language: lang-js -->

        // script tag src="/YourAppPath/scripts/jquery-1.6.4.js"
        // script tag src="/YourAppPath/scripts/jquery.signalR-2.2.0.js"
        // script tag src="/YourAppPath/signalr/hubs"

 5. Since SignalR's JavaScript is built on top of jQuery (requires >= v1.6.4), the code for connecting and disconnecting to the hub should look fairly trivial. Here it is in all its' glory (wrapped in an IIFE) :

    <!-- language: lang-js -->

        $(function() {
            //notice the camel casing of our hub name corresponding to the [HubName()] attribute on the server
            var connection = $.connection.messagingHub;
    
            $.connection.hub.start().done(function () {
                alert("Connection Successful!");
            }).fail(function (reason) {
                console.log("SignalR connection failed: " + reason);
            });
        });

 6. As of right now, we should be able to run the app and establish a connection to the SignalR hub.


## Using SignalR with Web API and JavaScript Web App, with CORS support.
**Objective:** Use SignalR for notification between Web API, and TypeScript/JavaScript based Web App, where Web API and the Web App is hosted in different domain.

**Enabling SignalR and CORS on Web API:**
Create a standard Web API project, and install the following NuGet packages:

 - Microsoft.Owin.Cors
 - Microsoft.AspNet.WebApi.Cors
 - Microsoft.AspNet.WebApi.Owin
 - Microsoft.AspNet.SignalR.Core

After that you can get rid of the `Global.asax` and add a OWIN Startup class instead.

    using System.Web.Http;
    using System.Web.Http.Cors;
    using Microsoft.Owin;
    using Owin;

    [assembly: OwinStartup(typeof(WebAPI.Startup), "Configuration")]
    namespace WebAPI
    {
        public class Startup
        {
            public void Configuration(IAppBuilder app)
            {
            
                var httpConfig = new HttpConfiguration();
                
                //change this configuration as you want.
                var cors = new EnableCorsAttribute("http://localhost:9000", "*", "*"); 
                httpConfig.EnableCors(cors);

                SignalRConfig.Register(app, cors);

                WebApiConfig.Register(httpConfig);

                app.UseWebApi(httpConfig);
            }
        }
    }

Create the `SignalRConfig` class as follows:

    using System.Linq;
    using System.Threading.Tasks;
    using System.Web.Cors;
    using System.Web.Http.Cors;
    using Microsoft.Owin.Cors;
    using Owin;

    namespace WebAPI
    {
        public static class SignalRConfig
        {
            public static void Register(IAppBuilder app, EnableCorsAttribute cors)
            {
    
                app.Map("/signalr", map =>
                {
                    var corsOption = new CorsOptions
                    {
                        PolicyProvider = new CorsPolicyProvider
                        {
                            PolicyResolver = context =>
                            {
                                var policy = new CorsPolicy { AllowAnyHeader = true, AllowAnyMethod = true, SupportsCredentials = true };
    
                                // Only allow CORS requests from the trusted domains.
                                cors.Origins.ToList().ForEach(o => policy.Origins.Add(o));
    
                                return Task.FromResult(policy);
                            }
                        }
                    };
                    map.UseCors(corsOption).RunSignalR();
                });
            }
        }
    }

Till now we have just enabled SignalR with CORS on server side. Now lets see how you can publish events from server side. For this we need a `Hub`:

    public class NotificationHub:Hub
    {
        //this can be in Web API or in any other class library that is referred from Web API.
    }

Now finally some code to actually broadcast the change:

    public class SuperHeroController : ApiController
    {
        [HttpGet]
        public string RevealAlterEgo(string id)
        {
            var alterEgo = $"The alter ego of {id} is not known.";
            var superHero = _superHeroes.SingleOrDefault(sh => sh.Name.Equals(id));
            if (superHero != null)
            {
                alterEgo = superHero.AlterEgo;
                
                /*This is how you broadcast the change. 
                 *For simplicity, in this example, the broadcast is done from a Controller, 
                 *but, this can be done from any other associated class library having access to NotificationHub.
                 */
                var notificationHubContext = GlobalHost.ConnectionManager.GetHubContext<NotificationHub>();
                if (notificationHubContext != null)
                {
                    var changeData = new { changeType = "Critical", whatHappened = $"Alter ego of {id} is revealed." };

                    //somethingChanged is an arbitrary method name.
                    //however, same method name is also needs to be used in client.
                    notificationHubContext.Clients.All.somethingChanged(changeData);
                }
            }
            return alterEgo;
        }
    }

Thus, so far, we made the server side ready. For client side, we need `jQuery`, and [`signalr`][1] package. You may install both with `jspm`. Install the typings for both, if needed.

We will not be using the default generated JavaScript proxy. We will rather create a very simple class to handle the SignalR communication.

    
    /**
     * This is created based on this gist: https://gist.github.com/donald-slagle/bf0673b3c188f3a2559c.
     * As we are crreating our own SignalR proxy, 
     * we don't need to get the auto generated proxy using `signalr/hubs` link.
     */
    export class SignalRClient {
    
        public connection = undefined;
        private running: boolean = false;
    
        public getOrCreateHub(hubName: string) {
            hubName = hubName.toLowerCase();
            if (!this.connection) {
                this.connection = jQuery.hubConnection("https://localhost:44378");
            }
    
            if (!this.connection.proxies[hubName]) {
                this.connection.createHubProxy(hubName);
            }
    
            return this.connection.proxies[hubName];
        }
    
        public registerCallback(hubName: string, methodName: string, callback: (...msg: any[]) => void,
            startIfNotStarted: boolean = true) {
    
            var hubProxy = this.getOrCreateHub(hubName);
            hubProxy.on(methodName, callback);

            //Note: Unlike C# clients, for JavaScript clients, 
            //      at least one callback needs to be registered, 
            //      prior to start the connection.
            if (!this.running && startIfNotStarted)
                this.start();
        }
    
        start() {
            const self = this;
            if (!self.running) {
                self.connection.start()
                    .done(function () {
                        console.log('Now connected, connection Id=' + self.connection.id);
                        self.running = true;
                    })
                    .fail(function () {
                        console.log('Could not connect');
                    });
            }
        }
    }

Lastly use this class to listen to change broadcasts, as follows:

    /**
     * Though the example contains Aurelia codes, 
     * the main part of SignalR communication is without any Aurelia dependency.
     */
    import {autoinject, bindable} from "aurelia-framework";
    import {SignalRClient} from "./SignalRClient";
    
    @autoinject
    export class SomeClass{
        
        //Instantiate SignalRClient.
        constructor(private signalRClient: SignalRClient) {
        }
    
        attached() {
            //To register callback you can use lambda expression...
            this.signalRClient.registerCallback("notificationHub", "somethingChanged", (data) => {
                console.log("Notified in VM via signalr.", data);
            });
            
            //... or function name.
            this.signalRClient.registerCallback("notificationHub", "somethingChanged", this.somethingChanged);
        }
    
        somethingChanged(data) {
            console.log("Notified in VM, somethingChanged, via signalr.", data);
        }
    }
    
  [1]: https://www.npmjs.com/package/signalr

