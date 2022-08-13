---
title: "Configuration"
slug: "configuration"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Asp.net core provides configuration abstractions. They allow you to load configuration settings from various sources and build a final configuration model which can then be consumed by your application.

## Syntax
* `IConfiguration`
* `string this[string key] { get; set; }`
* `IEnumerable<IConfigurationSection> GetChildren();`
* `IConfigurationSection GetSection(string key);`

## Getting Started
In this example we will describe what happens when you scaffold a new project.

First thing, the following dependencies will be added to you project (currently `project.json` file) :
```
"Microsoft.Extensions.Configuration.EnvironmentVariables": "1.0.0",
"Microsoft.Extensions.Configuration.Json": "1.0.0",
```

It will also create a constructor in your `Startup.cs` file which will be in charge of building the configuration using `ConfigurationBuilder` fluent api:
1. It first creates a new `ConfigurationBuilder`.
2. It then sets a base path which will be used to compute absolute path of further files
3. It adds an optional `appsettings.json` to the configuration builder and monitor it's changes
4. It adds an optional environment related `appsettings.environementName.json` configuration file
5. It then adds environement variables.

```csharp
public Startup(IHostingEnvironment env)
{
    var builder = new ConfigurationBuilder()
        .SetBasePath(env.ContentRootPath)
        .AddJsonFile("appsettings.json", optional: true, reloadOnChange: true)
        .AddJsonFile($"appsettings.{env.EnvironmentName}.json", optional: true)
        .AddEnvironmentVariables();

    Configuration = builder.Build();
}
```

If a same setting is set in several sources, the latest source added will win and its value will be selected.

Configuration can then be consumed using the indexer property. The colon `:` character serve a path delimiter.
```
Configuration["AzureLogger:ConnectionString"]
```

This will look for a configuration value `ConnectionString` in an `AzureLogger` section.

## Work with Environment Variables
You can source configuration from environment variables by calling `.AddEnvironmentVariables()` on you `ConfigurationBuilder`.

It will load environment variables prefixed with `APPSETTING_` 
It will then use colon `:` as the key path separator.

This means that : following environement settings :
```
APPSETTING_Security:Authentication:UserName = a_user_name
APPSETTING_Security:Authentication:Password = a_user_password
```

Will be the equivalent this json :
```
{
    "Security" : {
       "Authentication" : {
           "UserName" : "a_user_name",
           "Password" : "a_user_password" 
        } 
    }
}
```

** Note that Azure Service will transmit settings as environment variables. Prefix will be set for you transparently. So to do the same in Azure just set two Application Settings in AppSettings blade :
```
Security:Authentication:UserName         a_user_name
Security:Authentication:Password         a_user_password
```

## Option model and configuration
When dealing with large configuration sets of value, it might become quite unhandy to load them one buy one. 

Option model which comes with asp.net offers a convenient way to map a `section` to a dotnet `poco`:
For instance, one might hydrate `StorageOptions` directly from a configuration section b adding `Microsoft.Extensions.Options.ConfigurationExtensions` package and calling the `Configure<TOptions>(IConfiguration config)` extension method.
```
services.Configure<StorageOptions>(Configuration.GetSection("Storage"));
```

## In Memory configuration source
You can also source configuration from an in memory object such as a `Dictionary<string,string>` 

```
.AddInMemoryCollection(new Dictionary<string, string>
{
    ["akey"] = "a value"
})
```

This can reveal helpful in integration/unit testing scenarios.

## Accessing Configuration using Dependency Injection
The recommended approach would be to avoid doing so and rather use `IOptions<TOptions>` and `IServiceCollection.Configure<TOptions>`. 

That said, this is still pretty straightforward to make `IConfigurationRoot`available application wide.

In the Startup.cs constructor you should have the following code to build the configuration,

     Configuration = builder.Build();

Here `Configuration` is an instance of `IConfigurationRoot`,
And add this instance as a Singleton to the service collection in ConfigureServices method , Startup.cs ,

 

     public void ConfigureServices(IServiceCollection services)
     {
        services.AddSingleton<IConfigurationRoot>(provider => Configuration);

For example, you can now access the configuration in a Controller/Service 

      public MyController(IConfigurationRoot config){
          var setting1= config.GetValue<string>("Setting1")
      }

