---
title: "Getting started with asp.net-core"
slug: "getting-started-with-aspnet-core"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
**Installing Visual Studio**
----------------------------

If you do not have Visual Studio installed, you can [download the free Visual Studio Community Edition here][1]. If you already have it installed, you can proceed to the next step.

**Creating an ASP.NET Core MVC Application.**
------------------------------------------------

 1. **Open Visual Studio.**
 2. **Select File > New Project.**
 3. **Select Web under the language of your choice** within the Templates section on the left.
 4. **Choose a preferred Project type** within the dialog.
 5. **Optional: Choose a .NET Framework you would like to target** 
 6. **Name your project** and indicate if you want to create a Solution for the project.
 7. **Click OK** to create the project.

[![enter image description here][2]][2]

You will be presented with another dialog to select the template you want to use for the project :

[![enter image description here][3]][3]

Each of the descriptions are self-explanatory. For this first project, **select Web Application**, which will contain all of the default configurations, authentication, and some existing content. 

Since this is an introduction application and doesn't require any security or authentication, you can **change the authentication option to No Authentication** on the right-side of the dialog and **click OK to create the project**.

You should then see the new project within the Solution Explorer :

[![enter image description here][4]][4]

**Press the F5 key to run the application** and begin a debugging session, which will launch the application within your default browser :

[![enter image description here][5]][5]

You can now see that your project is up and running locally and is ready as a starting point for you to build your application.


  [1]: https://www.visualstudio.com/en-us/products/visual-studio-community-vs.aspx
  [2]: http://i.stack.imgur.com/pMtnk.png
  [3]: http://i.stack.imgur.com/uOxqu.png
  [4]: http://i.stack.imgur.com/K0Fx5.png
  [5]: http://i.stack.imgur.com/h93dH.png

## Minimal ASP.NET Core Web API with ASP.NET Core MVC
<!-- language-all: c# -->
With ASP.NET Core 1.0, the MVC and Web API framework have been merged into one framework called ASP.NET Core MVC. This is a good thing, since MVC and Web API share a lot of functionality, yet there always were subtle differences and code duplication.

However, merging these two into framework one also made it more difficult to distinguish one from another. For example, the `Microsoft.AspNet.WebApi` represents the Web API 5.x.x framework, not the new one. But, when you include `Microsoft.AspNetCore.Mvc` (version `1.0.0`), you get the full blown package. This will contain _all_ the out-of-the-box features the MVC framework offers. Such as Razor, tag helpers and model binding.

When you just want to build a Web API, we don't need all this features. So, how do we build a minimalistic Web API? The answer is: [`Microsoft.AspNetCore.Mvc.Core`](https://www.nuget.org/packages/Microsoft.AspNetCore.Mvc.Core). In the new world MVC is split up into multiple packages and this package contains just the core components of the MVC framework, such as routing and authorization.

For this example, we're gonna create a minimal MVC API. Including a JSON formatter and CORS. Create an empty ASP.NET Core 1.0 Web Application and add these packages to your project.json:

<!-- language: json -->
    "Microsoft.AspNetCore.Mvc.Core": "1.0.0",
    "Microsoft.AspNetCore.Mvc.Cors": "1.0.0",
    "Microsoft.AspNetCore.Mvc.Formatters.Json": "1.0.0"

Now we can register MVC using `AddMvcCore()` in the startup class:

    public void ConfigureServices(IServiceCollection services)
    {
        services.AddMvcCore()
                .AddCors()
                .AddJsonFormatters();
    }

`AddMvcCore` returns an `IMvcCoreBuilder` instance which allows further building. Configuring the middleware is the same as usual:


    public void Configure(IApplicationBuilder app)
    {
        app.UseCors(policy =>
        {
            policy.AllowAnyOrigin();
        });
        app.UseMvc();
    }

## Controllers
The 'old' Web API comes with its own controller base class: `ApiController`. In the new world there is no such thing, only the default `Controller` class. Unfortunately, this is a rather large base class and it's tied to model binding, views and JSON.NET.

Fortunately, in the new framework controller classes don't have to derive from `Controller` to be picked up by the routing mechanism. Just appending the name with `Controller` is enough. This allows us to build our own controller base class. Let's call it `ApiController`, just for old times sake:

    /// <summary>
    /// Base class for an API controller.
    /// </summary>
    [Controller]
    public abstract class ApiController
    {
        [ActionContext]
        public ActionContext ActionContext { get; set; }
    
        public HttpContext HttpContext => ActionContext?.HttpContext;
    
        public HttpRequest Request => ActionContext?.HttpContext?.Request;
    
        public HttpResponse Response => ActionContext?.HttpContext?.Response;
    
        public IServiceProvider Resolver => ActionContext?.HttpContext?.RequestServices;
    }

The `[Controller]` attribute indicates that the type or any derived type is considered as a controller by the default controller discovery mechanism. The `[ActionContext]` attribute specifies that the property should be set with the current `ActionContext` when MVC creates the controller. The `ActionContext` provides information about the current request.

> ASP.NET Core MVC also offers a [`ControllerBase`](https://github.com/aspnet/Mvc/blob/1.0.0/src/Microsoft.AspNetCore.Mvc.Core/ControllerBase.cs) class which provides a controller base class just without views support. It's still much larger than ours though. Use it if you find it convenient.

## Conclusion
We can now build a minimal Web API using the new ASP.NET Core MVC framework. The modular package structure allows us to just pull in the packages we need and create a lean and simple application.

## Create a new project from the command line
It's possible to create a new ASP.NET Core project entirely from the command line using the `dotnet` command.

    dotnet new web
    dotnet restore
    dotnet run

`dotnet new web` scaffolds a new "empty" web project. The `web` parameter tells the `dotnet` tool to use the `ASP.NET Core Empty` template. Use `dotnet new -all` to show all the available templates currently installed. Other key templates include `console`, `classlib`, `mvc` and `xunit`.

Once the template has been scaffolded out, you can restore the packages required to run the project (`dotnet restore`), and compile and start it (`dotnet run`).

Once the project is running, it will be available on the default port: http://localhost:5000

## Using Visual Studio code to develop Cross plateform aspnet core application
With AspNetCore you can develop the application on any platform including Mac,Linux,Window and Docker.

**Installation and SetUp**

 1. Install visual Studio Code from [here][1]
 2. Add [C# extesnion][2]
 3. Install dot net core sdk. You can install from [here][3]

Now you have all the tools available. To develop the application. Now you need some scaffolding option. For that you should consider using Yeoman. To install Yeoman

 1. Install NPM. For this you need Node on your machine. Install from  [here][4]
 2. Install Yeoman by using NPM   

    npm install -g yo

 3. Now install the aspnet generator

    npm install -g generator-aspnet

Now we have all the setup on your machine. First let's create a new project with DotNetCore basic command and then create a new project using Yo.

**New Project Using Command Line**

 1. Create a new Project Folder

    mkdir CoreApplication 
    cd CoreApplication

 2. Scaffold a very basic dotnet project using default command line option 

    dotnet New

[![New Project][5]][5]

 1. Restore the packages and run the application 

    dotNet restore
    dotnet run
[![enter image description here][6]][6]


**Use Yeoman as Scaffolding Option**

 Create Project Folder and Run the Yo Command

    yo aspnet

Yeoman will ask some inputs like Project Type, Project Name etc like

[![enter image description here][7]][7]

[![enter image description here][8]][8]

Now restore the packages by running dotnet restore command and Run the application

**Use VS Code to develop the application**

Run the visual studio code like 
[![enter image description here][9]][9]


Now open the files and run the application. You can also search the extension for your help.

  [1]: https://code.visualstudio.com
  [2]: https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp
  [3]: https://www.microsoft.com/net/core#windows
  [4]: https://nodejs.org/en/
  [5]: http://i.stack.imgur.com/wadsM.png
  [6]: http://i.stack.imgur.com/Tx7v4.png
  [7]: http://i.stack.imgur.com/PoWbv.png
  [8]: http://i.stack.imgur.com/77SSC.png
  [9]: http://i.stack.imgur.com/W6Wpf.png

## Setup environment variable in ASP.NET Core [Windows]
[=> Original Post <=][1]

ASP.NET Core uses the `ASPNETCORE_ENVIRONMENT` environment variable to determine the current environment. By default, if you run your application without setting this value, it will automatically default to the `Production` environment.

    > dotnet run
    Project TestApp (.NETCoreApp,Version=v1.0) was previously compiled. Skipping compilation.

    Hosting environment: Production  
    Content root path: C:\Projects\TestApp  
    Now listening on: http://localhost:5000  
    Application started. Press Ctrl+C to shut down. 

**Setting the environment variable in Windows**

***At the command line***

You can easily set an environment variable from a command prompt using the setx.exe command included in Windows. You can use it to easily set a user variable:

    >setx ASPNETCORE_ENVIRONMENT "Development"

    SUCCESS: Specified value was saved.

Note that the environment variable is not set in the current open window. You will need to open a new command prompt to see the updated environment. It is also possible to set system variables (rather than just user variables) if you open an administrative command prompt and add the /M switch:

    >setx ASPNETCORE_ENVIRONMENT "Development" /M

    SUCCESS: Specified value was saved.

***Using PowerShell***
Alternatively, you can use PowerShell to set the variable. In PowerShell, as well as the normal user and system variables, you can also create a temporary variable using the `$Env:` command:

    $Env:ASPNETCORE_ENVIRONMENT = "Development"
The variable created lasts just for the duration of your PowerShell session - once you close the window the environment reverts back to its default value.

Alternatively, you could set the user or system environment variables directly. This method does not change the environment variables in the current session, so you will need to open a new PowerShell window to see your changes. As before, changing the system (Machine) variables will require administrative access

    [Environment]::SetEnvironmentVariable("ASPNETCORE_ENVIRONMENT", "Development", "User")
    [Environment]::SetEnvironmentVariable("ASPNETCORE_ENVIRONMENT", "Development", "Machine")

***Using the windows control panel***
If you're not a fan of the command prompt, you can easily update your variables using your mouse!Click the windows start menu button (or press the Windows key), search for `environment variables`, and choose *Edit environment variables for your account:*
[![enter image description here][2]][2]


Selecting this option will open the System Properties dialog
[![enter image description here][3]][3]

Click Environment Variables to view the list of current environment variables on your system.
[![enter image description here][4]][4]

Assuming you do not already have a variable called `ASPNETCORE_ENVIRONMENT`, click the New... button and add a new account environment variable:
[![enter image description here][5]][5]
Click OK to save all your changes. You will need to re-open any command windows to ensure the new environment variables are loaded.


  [1]: https://andrewlock.net/how-to-set-the-hosting-environment-in-asp-net-core/
  [2]: https://i.stack.imgur.com/g71hq.png
  [3]: https://i.stack.imgur.com/TCirT.png
  [4]: https://i.stack.imgur.com/ZL6Hv.png
  [5]: https://i.stack.imgur.com/yB7sd.png

