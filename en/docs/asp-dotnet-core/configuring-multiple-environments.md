---
title: "Configuring multiple Environments"
slug: "configuring-multiple-environments"
draft: false
images: []
weight: 9904
type: docs
toc: true
---

## Having appsettings per Environment
For each environment you need to create a separate appsettings.{EnvironmentName}.json files:
 - appsettings.Development.json
 - appsettings.Staging.json
 - appsettings.Production.json

Then open project.json file and include them into "include" in "publishOptions" section. This lists all the files and folders that will be included when you publish:

    "publishOptions": {
      "include": [
        "appsettings.Development.json",
        "appsettings.Staging.json",
        "appsettings.Production.json"
        ... 
      ]
    }

The final step. In your Startup class add:

    .AddJsonFile($"appsettings.{env.EnvironmentName}.json", optional: true);

in constructor where you set up configuration sources:

    var builder = new ConfigurationBuilder()
                .SetBasePath(env.ContentRootPath)
                .AddJsonFile($"appsettings.{env.EnvironmentName}.json", optional: true)
                .AddEnvironmentVariables();


## Get/Check Environment name from code
All you need is a variable of type `IHostingEnvironment`:

- get environment name: 

        env.EnvironmentName

- for predefined `Development`, `Staging`, `Production` environments the best way is to use extension methods from [HostingEnvironmentExtensions][1] class 

        env.IsDevelopment()
        env.IsStaging()
        env.IsProduction()

- correctly ignore case (another extension method from [HostingEnvironmentExtensions][1]: 

       env.IsEnvironment("environmentname") 

- case sensitive variant: 

       env.EnvironmentName == "Development" 





  [1]: https://github.com/aspnet/Hosting/blob/b7bdc9c40494f8e0f0eac22db91b8d5c58811ee2/src/Microsoft.AspNetCore.Hosting.Abstractions/HostingEnvironmentExtensions.cs

## Configuring multiple environments
This example shows how to configure multiple environments with different Dependency Injection configuration and separate middlewares in one `Startup` class.

Alongside of `public void Configure(IApplicationBuilder app)` and `public void ConfigureServices(IServiceCollection services)` methods one can use `Configure{EnvironmentName}` and `Configure{EnvironmentName}Services` to have environment dependent configuration. 

<!-- language-all: csharp -->

Using this pattern avoids putting to much `if/else` logic withing one single method/`Startup` class and keep it clean and separated. 

    public class Startup
    {
        public void ConfigureServices(IServiceCollection services) { }
        public void ConfigureStaggingServices(IServiceCollection services) { }
        public void ConfigureProductionServices(IServiceCollection services) { }
        
        public void Configure(IApplicationBuilder app) { }
        public void ConfigureStagging(IApplicationBuilder app) { }
        public void ConfigureProduction(IApplicationBuilder app) { }
    }

When a `Configure{Environmentname}` or `Configure{Environmentname}Services` is not found, it will fall back to `Configure` or `ConfigureServices` respectively.     

The same semantics also apply to the `Startup` class. `StartupProduction` will be used when the `ASPNETCORE_ENVIRONMENT` variable is set to `Production` and will fall back to `Startup` when it's `Stagging` or `Development`

A complete example: 

    public class Startup
    {
        public Startup(IHostingEnvironment hostEnv)
        {
            // Set up configuration sources.
            var builder = new ConfigurationBuilder()
                .SetBasePath(hostEnv.ContentRootPath)
                .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true)
                .AddJsonFile($"appsettings.{hostEnv.EnvironmentName}.json", optional: true, reloadOnChange: true);

            if (hostEnv.IsDevelopment())
            {
                // This will push telemetry data through Application Insights pipeline faster, allowing you to view results immediately.
                builder.AddApplicationInsightsSettings(developerMode: true);
            }

            builder.AddEnvironmentVariables();
            Configuration = builder.Build();
        }

        public IConfigurationRoot Configuration { get; set; }

        // This method gets called by the runtime. Use this method to add services to the container
        public static void RegisterCommonServices(IServiceCollection services) 
        {
            services.AddScoped<ICommonService, CommonService>();
            services.AddScoped<ICommonRepository, CommonRepository>();
        }
        
        public void ConfigureServices(IServiceCollection services)
        {
            RegisterCommonServices(services);
            
            services.AddOptions();
            services.AddMvc();
        }

        public void ConfigureDevelopment(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory)
        {
            loggerFactory.AddConsole(Configuration.GetSection("Logging"));
            loggerFactory.AddDebug();

            app.UseBrowserLink();
            app.UseDeveloperExceptionPage();

            app.UseApplicationInsightsRequestTelemetry();
            app.UseApplicationInsightsExceptionTelemetry();
            app.UseStaticFiles();
            app.UseMvc();
        }

        // No console Logger and debugging tools in this configuration
        public void Configure(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory)
        {
            loggerFactory.AddDebug();

            app.UseApplicationInsightsRequestTelemetry();
            app.UseApplicationInsightsExceptionTelemetry();
            app.UseStaticFiles();
            app.UseMvc();
        }
    }

## Render environment specific content in view
You may need to render some content in view, which is specific to some environment only. To achieve this goal you can use Environment [tag helper](https://www.wikiod.com/asp-dotnet-core/tag-helpers):

<!-- language-all: lang-razor -->

    <environment names="Development">
        <h1>This is heading for development environment</h1>
    </environment>
    <environment names="Staging,Production">
        <h1>This is heading for Staging or production environment</h1>
    </environment>

The Environment tag helper will only render its contents if the current environment matches one of the environments specified using the `names` attribute.

## Set environment variable from command line
To set the environment to `Development`

`SET ASPNETCORE_ENVIRONMENT=Development`

Now running an Asp.Net Core application will be in the defined environment.

**Note**  
1. There should be no space before and after the equality sign `=`.
2. The command prompt should not be closed before running the application because the settings are not persisted.

## Set environment variable from PowerShell
When using PowerShell, you can use setx.exe to set environment variables permanently. 

1. Start PowerShell
2. Type one of the following:

    setx ASPNETCORE_ENVIRONMENT "development"

    setx ASPNETCORE_ENVIRONMENT "staging"

3. Restart PowerShell

## Using ASPNETCORE_ENVIRONMENT from web.config
If you do not want to use ASPNETCORE_ENVIRONMENT from environment variables and use it from web.config of your application then modify web.config like this-

    <aspNetCore processPath=".\WebApplication.exe" arguments="" stdoutLogEnabled="false" stdoutLogFile=".\logs\stdout" forwardWindowsAuthToken="false">
      <environmentVariables>
        <environmentVariable name="ASPNETCORE_ENVIRONMENT" value="Development" />
      </environmentVariables>
    </aspNetCore>



