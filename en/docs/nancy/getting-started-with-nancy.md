---
title: "Getting started with nancy"
slug: "getting-started-with-nancy"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Create a simple self-hosted Nancy application
 1. Use Nuget to install the Nancy and Nancy.Hosting.Self packages into the project.
 2. Instantiate a new NancyHost object and pass in the relevant URL
<pre>
using( var host = new NancyHost( hostConfiguration, new Uri( "http://localhost:1234" ) ) )
{
    host.Start();
    Console.WriteLine( "Running on http://localhost:1234" );
    Console.ReadLine();
}</pre>
*Place this code in your project at the point when you wish to start listening for http traffic.*
3. Add a class to your project that inherits from NancyModule and add a constructor method.


    public class FooModule : NancyModule
    {
        public FooModule()
        {
        }
    }

4. Define routes in the constructor:


    ...
    public FooModule()
    {
        Get["Bar"] = parameters => {
            return "You have reached the /bar route";
        }
    }


## Setup Nancyfx with Dotnet core v1.1, Kestrel, and Visual Studio Code on *nix systems

## Prerequiste steps:

<ol>
<li>Get dotnet core for your platform:

[Dotnet Core](https://www.microsoft.com/net/core)</li>
<li>Follow instructions and make sure dotnet core is working</li>
<li>Get Visual Studio Code for your platform: 

[VS Code](https://code.visualstudio.com)</li>
<li>Launch Visual Studio Code (VS code) and install the C# extension then reload</li>
</ol>

## Create self hosted NancyFx project:

<ol>
<li>
Setup a project with a correct project directory structure.

Open Bash Terminal and type:

<pre>
mkdir nancydotnetcore
cd nancydotnetcore
mkdir src 
mkdir test
touch global.json
</pre>
</li>
<li>
Open global.json and enter the following code:

<pre>
{
    "projects":["src", "test"]
}
</pre>
</li>
<li>
In Bash terminal:

<pre>
cd src 
mkdir NancyProject1
dotnet new 
</pre>

Open folder NancyProject1 in VS code

You will get a warning: "Required assets to build and debug are missing from 'nancyproject1'."

Click "Yes"

Also you will see: There are unresolved dependencies from 'project.json'. Please execute the restore command to continue.

Click "Close" we will get to this soon. 
</li>
<li>
Add the dependencies, open "project.json" and overwrite it with the following:

<pre>
{
    "version": "1.0.0-*",
    "buildOptions": {
        "debugType": "portable",
        "emitEntryPoint": true
    },

    "frameworks": {
        "netcoreapp1.1": {
            "dependencies": {
                "Microsoft.AspNetCore.Hosting": "1.1.0",
                "Microsoft.AspNetCore.Server.Kestrel": "1.1.0",
                "Microsoft.AspNetCore.Owin": "1.1.0",
                "Nancy": "2.0.0-barneyrubble",
                "Microsoft.NETCore.App": {
                    "type": "platform",
                    "version": "1.1.0"
                }
            }
        }
    }
}
</pre>

VS code will ask to restore click "Restore"
</li>
<li>
Create folder "Modules" in VSCode project

In the Modules folder add a file named "IndexModule.cs" then copy and save the following:

<pre>
namespace NancyProject1
{
    using Nancy;
    public class IndexModule : NancyModule
    {
        public IndexModule()
        {
            Get("/", _ => "Hello dotnet core world!");
        }
    }
}
</pre>

</li>
<li>
In the root directory of the project create a file called "Startup.cs" and copy and paste the following:

<pre>
namespace NancyProject1
{
    using Microsoft.AspNetCore.Builder;
    using Nancy.Owin;

    public class Startup
    {
        public void Configure(IApplicationBuilder app)
        {
            app.UseOwin(x => x.UseNancy());
        }
    }
}
</pre>

</li>
<li>

Open file "Program.cs" and overwrite the content with the following and save:


    namespace NancyProject1
    {
        using System.IO;
        using Microsoft.AspNetCore.Builder;
        using Microsoft.AspNetCore.Hosting;
    
        public class Program
        {
            public static void Main(string[] args)
            {
                var host = new WebHostBuilder()
                    .UseContentRoot(Directory.GetCurrentDirectory())
                    .UseKestrel()
                    .UseStartup<Startup>()
                    .Build();
    
                host.Run();
            }
        }
    }

</li>
<li>
Done! Now lets run this and see the output. 

Click the debug symbol in VS Code, and Click the run button. It should compile and start the project.

Open the browser @ http://localhost:5000
</li>
<li>
Pat yourself on the back and enjoy! 
</li>
</ol>



