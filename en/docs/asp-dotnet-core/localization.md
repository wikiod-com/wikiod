---
title: "Localization"
slug: "localization"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

## Localization using JSON language resources
<!-- language-all: c# -->
In ASP.NET Core there are several different ways we can localize/globalize our app. It's important to pick a way that suits your needs. In this example you'll see how we can make a multilingual ASP.NET Core app that reads language specific strings from `.json` files and store them in memory to provide localization in all sections of the app as well as  maintaining a high performance.

The way we do it is by using `Microsoft.EntityFrameworkCore.InMemory` package.

Notes:

 1. The namespace for this project is `DigitalShop` that you may change to your projects own namespace
 2. Consider creating a new project so that you don't run into weird errors
 3. By no means this example show the best practices, So if you think it can be improved please kindly edit it

To begin let's add the following packages to the **existing** `dependencies` section in the `project.json` file:

    "Microsoft.EntityFrameworkCore": "1.0.0",
    "Microsoft.EntityFrameworkCore.SqlServer": "1.0.0",
    "Microsoft.EntityFrameworkCore.InMemory": "1.0.0"

Now let's replace the `Startup.cs` file with: (`using` statements are removed as they can be easily added later)

**Startup.cs**
    
    namespace DigitalShop
    {
        public class Startup
        {
            public static string UiCulture;
            public static string CultureDirection;
            public static IStringLocalizer _e; // This is how we access language strings
    
            public static IConfiguration LocalConfig;
    
            public Startup(IHostingEnvironment env)
            {
                var builder = new ConfigurationBuilder()
                    .SetBasePath(env.ContentRootPath)
                    .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true) // this is where we store apps configuration including language
                    .AddJsonFile($"appsettings.{env.EnvironmentName}.json", optional: true)
                    .AddEnvironmentVariables();
    
                Configuration = builder.Build();
                LocalConfig = Configuration;
            }
    
            public IConfigurationRoot Configuration { get; }
    
            // This method gets called by the runtime. Use this method to add services to the container.
            public void ConfigureServices(IServiceCollection services)
            {
                services.AddMvc().AddViewLocalization().AddDataAnnotationsLocalization();
    
                // IoC Container
                // Add application services.
                services.AddTransient<EFStringLocalizerFactory>();
                services.AddSingleton<IConfiguration>(Configuration);
            }
    
            // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
            public void Configure(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory, EFStringLocalizerFactory localizerFactory)
            {
                _e = localizerFactory.Create(null);
    
                // a list of all available languages
                var supportedCultures = new List<CultureInfo>
                {
                    new CultureInfo("en-US"),
                    new CultureInfo("fa-IR")
                };
    
                var requestLocalizationOptions = new RequestLocalizationOptions
                {
                    SupportedCultures = supportedCultures,
                    SupportedUICultures = supportedCultures,
                };
                requestLocalizationOptions.RequestCultureProviders.Insert(0, new JsonRequestCultureProvider());
                app.UseRequestLocalization(requestLocalizationOptions);
    
                app.UseStaticFiles();
    
                app.UseMvc(routes =>
                {
                    routes.MapRoute(
                        name: "default",
                        template: "{controller=Home}/{action=Index}/{id?}");
                });
            }
        }
    
        public class JsonRequestCultureProvider : RequestCultureProvider
        {
            public override Task<ProviderCultureResult> DetermineProviderCultureResult(HttpContext httpContext)
            {
                if (httpContext == null)
                {
                    throw new ArgumentNullException(nameof(httpContext));
                }
    
                var config = Startup.LocalConfig;
    
                string culture = config["AppOptions:Culture"];
                string uiCulture = config["AppOptions:UICulture"];
                string culturedirection = config["AppOptions:CultureDirection"];
    
                culture = culture ?? "fa-IR"; // Use the value defined in config files or the default value
                uiCulture = uiCulture ?? culture;
    
                Startup.UiCulture = uiCulture;
    
                culturedirection = culturedirection ?? "rlt"; // rtl is set to be the default value in case culturedirection is null
                Startup.CultureDirection = culturedirection;
    
                return Task.FromResult(new ProviderCultureResult(culture, uiCulture));
            }
        }
    }

In the above code, we first add three `public static` field variables that we will later initialize using the values read from the settings file.

In the constructor for `Startup` class we add a json settings file to the `builder` variable. The first file is required for the app to work, so go ahead and create `appsettings.json` in your project root if it doesn't already exist. Using Visual Studio 2015, this file is created automatically, so just change its content to: (You may omit the `Logging` section if you don't use it)

**appsettings.json**

    {
      "Logging": {
        "IncludeScopes": false,
        "LogLevel": {
          "Default": "Debug",
          "System": "Information",
          "Microsoft": "Information"
        }
      },
      "AppOptions": {
        "Culture": "en-US", // fa-IR for Persian
        "UICulture": "en-US", // same as above
        "CultureDirection": "ltr" // rtl for Persian/Arabic/Hebrew
      }
    }

Going forward, create three folders in your project root:

`Models`, `Services` and `Languages`. In the `Models` folder create another folder named `Localization`.

In the `Services` folder we create a new .cs file named `EFLocalization`. The content would be: (Again `using` statements are not included)

**EFLocalization.cs**

    namespace DigitalShop.Services
    {
        public class EFStringLocalizerFactory : IStringLocalizerFactory
        {
            private readonly LocalizationDbContext _db;
    
            public EFStringLocalizerFactory()
            {
                _db = new LocalizationDbContext();
                // Here we define all available languages to the app
                // available languages are those that have a json and cs file in
                // the Languages folder
                _db.AddRange(
                    new Culture
                    {
                        Name = "en-US",
                        Resources = en_US.GetList()
                    },
                    new Culture
                    {
                        Name = "fa-IR",
                        Resources = fa_IR.GetList()
                    }
                );
                _db.SaveChanges();
            }
    
            public IStringLocalizer Create(Type resourceSource)
            {
                return new EFStringLocalizer(_db);
            }
    
            public IStringLocalizer Create(string baseName, string location)
            {
                return new EFStringLocalizer(_db);
            }
        }
    
        public class EFStringLocalizer : IStringLocalizer
        {
            private readonly LocalizationDbContext _db;
    
            public EFStringLocalizer(LocalizationDbContext db)
            {
                _db = db;
            }
    
            public LocalizedString this[string name]
            {
                get
                {
                    var value = GetString(name);
                    return new LocalizedString(name, value ?? name, resourceNotFound: value == null);
                }
            }
    
            public LocalizedString this[string name, params object[] arguments]
            {
                get
                {
                    var format = GetString(name);
                    var value = string.Format(format ?? name, arguments);
                    return new LocalizedString(name, value, resourceNotFound: format == null);
                }
            }
    
            public IStringLocalizer WithCulture(CultureInfo culture)
            {
                CultureInfo.DefaultThreadCurrentCulture = culture;
                return new EFStringLocalizer(_db);
            }
    
            public IEnumerable<LocalizedString> GetAllStrings(bool includeAncestorCultures)
            {
                return _db.Resources
                    .Include(r => r.Culture)
                    .Where(r => r.Culture.Name == CultureInfo.CurrentCulture.Name)
                    .Select(r => new LocalizedString(r.Key, r.Value, true));
            }
    
            private string GetString(string name)
            {
                return _db.Resources
                    .Include(r => r.Culture)
                    .Where(r => r.Culture.Name == CultureInfo.CurrentCulture.Name)
                    .FirstOrDefault(r => r.Key == name)?.Value;
            }
        }
    
        public class EFStringLocalizer<T> : IStringLocalizer<T>
        {
            private readonly LocalizationDbContext _db;
    
            public EFStringLocalizer(LocalizationDbContext db)
            {
                _db = db;
            }
    
            public LocalizedString this[string name]
            {
                get
                {
                    var value = GetString(name);
                    return new LocalizedString(name, value ?? name, resourceNotFound: value == null);
                }
            }
    
            public LocalizedString this[string name, params object[] arguments]
            {
                get
                {
                    var format = GetString(name);
                    var value = string.Format(format ?? name, arguments);
                    return new LocalizedString(name, value, resourceNotFound: format == null);
                }
            }
    
            public IStringLocalizer WithCulture(CultureInfo culture)
            {
                CultureInfo.DefaultThreadCurrentCulture = culture;
                return new EFStringLocalizer(_db);
            }
    
            public IEnumerable<LocalizedString> GetAllStrings(bool includeAncestorCultures)
            {
                return _db.Resources
                    .Include(r => r.Culture)
                    .Where(r => r.Culture.Name == CultureInfo.CurrentCulture.Name)
                    .Select(r => new LocalizedString(r.Key, r.Value, true));
            }
    
            private string GetString(string name)
            {
                return _db.Resources
                    .Include(r => r.Culture)
                    .Where(r => r.Culture.Name == CultureInfo.CurrentCulture.Name)
                    .FirstOrDefault(r => r.Key == name)?.Value;
            }
        }
    }

In the above file we implement the `IStringLocalizerFactory` interface from Entity Framework Core in order to make a custom localizer service. The important part is the constructor of `EFStringLocalizerFactory` where we make a list of all available languages and add it to the database context. Each one of these language files act as a separate database.

Now add each of the following files to the `Models/Localization` folder:


**Culture.cs**

    namespace DigitalShop.Models.Localization
    {
        public class Culture
        {
            public int Id { get; set; }
            public string Name { get; set; }
            public virtual List<Resource> Resources { get; set; }
        }
    }

**Resource.cs**

    namespace DigitalShop.Models.Localization
    {
        public class Resource
        {
            public int Id { get; set; }
            public string Key { get; set; }
            public string Value { get; set; }
            public virtual Culture Culture { get; set; }
        }
    }

**LocalizationDbContext.cs**

    namespace DigitalShop.Models.Localization
    {
        public class LocalizationDbContext : DbContext
        {
            public DbSet<Culture> Cultures { get; set; }
            public DbSet<Resource> Resources { get; set; }
    
            protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
            {
                optionsBuilder.UseInMemoryDatabase();
            }
        }
    }

The above files are just models that will be populated with language resources, cultures and there's also a typical `DBContext` used by EF Core.

The last thing we need to make all of this work is to create the language resource files. The JSON files used to store a key-value pair for different languages available in your app.

In this example our app only has two languages available. English and Persian. For each of the languages we need two files. A JSON file containing key-value pairs and a `.cs` file that contains a class with the same name as JSON file. That class has one method, `GetList` that deserializes the JSON file and returns it. This method is called in the constructor of `EFStringLocalizerFactory` that we created earlier.

So, create these four files in your `Languages` folder:

**en-US.cs**

    namespace DigitalShop.Languages
    {
        public static class en_US
        {
            public static List<Resource> GetList()
            {
                var jsonSerializerSettings = new JsonSerializerSettings();
                jsonSerializerSettings.MissingMemberHandling = MissingMemberHandling.Ignore;
                return JsonConvert.DeserializeObject<List<Resource>>(File.ReadAllText("Languages/en-US.json"), jsonSerializerSettings);
            }
        }
    }

**en-US.json**

    [
      {
        "Key": "Welcome",
        "Value": "Welcome"
      },
      {
        "Key": "Hello",
        "Value": "Hello"
      },
    ]

**fa-IR.cs**

    public static class fa_IR
    {
        public static List<Resource> GetList()
        {
            var jsonSerializerSettings = new JsonSerializerSettings();
            jsonSerializerSettings.MissingMemberHandling = MissingMemberHandling.Ignore;
            return JsonConvert.DeserializeObject<List<Resource>>(File.ReadAllText("Languages/fa-IR.json", Encoding.UTF8), jsonSerializerSettings);
        }
    }

**fa-IR.json**

    [
      {
        "Key": "Welcome",
        "Value": "خوش آمدید"
      },
      {
        "Key": "Hello",
        "Value": "سلام"
      },
    ]

We are all done. Now in order to access the language strings (key-value pairs) anywhere in your code (`.cs` or `.cshtml`) you can do the following:

in a `.cs` file (be Controller or not, doesn't matter):

    // Returns "Welcome" for en-US and "خوش آمدید" for fa-IR
    var welcome = Startup._e["Welcome"];

in a Razor view file (`.cshtml`):

    <h1>@Startup._e["Welcome"]</h1>

Few things to keep in mind:

 - If you try to access a `Key` that doesn't exist in the JSON file or loaded, you will just get the key literal (in the above example, trying to access `Startup._e["How are you"]` will return `How are you` no matter the language settings because it doesn't exist
 - If you change a string value in a language `.json` file, you will need to **RESTART** the app. Otherwise it will just show the default value (key name). **This is specially important when you're running your app without debugging.**
 - The `appsettings.json` can be used to store all kinds of settings that your app may need
 - Restarting the app is **not necessary** if you just want to change the **language/culture settings from `appsettings.json` file**. This means that you can have an option in your apps interface to let users change the language/culture at runtime.

Here's the final project structure:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/8I6TR.png

## Set Request culture via url path
By default the built-in Request Localization middleware only supports setting culture via query, cookie or `Accept-Language` header. This example shows how create a middleware which allows to set the culture as part of the path like in `/api/en-US/products`.

This example middleware assumes the locale to be in the second segment of the path. 

<!-- language-all: c# -->
    public class UrlRequestCultureProvider : RequestCultureProvider
    {
        private static readonly Regex LocalePattern = new Regex(@"^[a-z]{2}(-[a-z]{2,4})?$",
                                                            RegexOptions.IgnoreCase);

        public override Task<ProviderCultureResult> DetermineProviderCultureResult(HttpContext httpContext)
        {
            if (httpContext == null)
            {
                throw new ArgumentNullException(nameof(httpContext));
            }

            var url = httpContext.Request.Path;

            // Right now it's not possible to use httpContext.GetRouteData()
            // since it uses IRoutingFeature placed in httpContext.Features when
            // Routing Middleware registers. It's not set when the Localization Middleware
            // is called, so this example simply assumes the locale will always 
            // be located in the second segment of a path, like in /api/en-US/products
            var parts = httpContext.Request.Path.Value.Split('/');
            if (parts.Length < 3)
            {
                return Task.FromResult<ProviderCultureResult>(null);
            }

            if (!LocalePattern.IsMatch(parts[2]))
            {
                return Task.FromResult<ProviderCultureResult>(null);
            }

            var culture = parts[2];
            return Task.FromResult(new ProviderCultureResult(culture));
        }
    }

# Middleware Registration

    var localizationOptions = new RequestLocalizationOptions
    {
        SupportedCultures = new List<CultureInfo>
        {
            new CultureInfo("de-DE"),
            new CultureInfo("en-US"),
            new CultureInfo("en-GB")
        },
        SupportedUICultures = new List<CultureInfo>
        {
            new CultureInfo("de-DE"),
            new CultureInfo("en-US"),
            new CultureInfo("en-GB")
        },
        DefaultRequestCulture = new RequestCulture("en-US")
    };

    // Adding our UrlRequestCultureProvider as first object in the list
    localizationOptions.RequestCultureProviders.Insert(0, new UrlRequestCultureProvider
    {
        Options = localizationOptions
    });

    app.UseRequestLocalization(localizationOptions);

# Custom Route Constraints

Adding and creating custom route constraints are shown in the [Route constrains][1] example. Using constraints simplifies the usage of custom route constrains. 

# Registering the route

Example of registering the routes without using a custom constraints

    app.UseMvc(routes => 
    { 
        routes.MapRoute( 
            name: "default", 
            template: "api/{culture::regex(^[a-z]{{2}}-[A-Za-z]{{4}}$)}}/{controller}/{id?}"); 
        routes.MapRoute( 
            name: "default", 
            template: "api/{controller}/{id?}"); 
    });
    
  [1]: https://www.wikiod.com/asp-dotnet-core/routing#Routing constraints

