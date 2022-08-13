---
title: "Getting started with appsetting.json"
slug: "getting-started-with-appsettingjson"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

If you need more info, you can go and see [official microsoft documentation][1]


  [1]: https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration

## Simple configuration
Add this text to appsettings.json

    {
      "key1": "value1",
      "key2": 2,
    
      "subsectionKey": {
        "suboption1": "subvalue1"
      }
    }

Now you can use this configuration in your app, in the way like this

    public class Program
    {
        static public IConfigurationRoot Configuration { get; set; }
        public static void Main(string[] args = null)
        {
            var builder = new ConfigurationBuilder()
                 .SetBasePath(Directory.GetCurrentDirectory())
                .AddJsonFile("appsettings.json");
            Configuration = builder.Build();
    
            Console.WriteLine($"option1 = {Configuration["key1"]}");
            Console.WriteLine($"option2 = {Configuration["key2"]}");
            Console.WriteLine(
                $"option1 = {Configuration["subsectionKey:suboption1"]}");
        }
    }
    

## Using configuration object for settings
Create class like a class below

    public class MyOptions
    {
        public MyOptions()
        {
            // Set default value, if you need it.
            Key1 = "value1_from_ctor";
        }
        public string Key1 { get; set; }
        public int Key2 { get; set; }
    }

Then you need to add this code to your Startup class

    public class Startup
    {
        // Some default code here
    
        public IConfigurationRoot Configuration { get; set; }
    
        public void ConfigureServices(IServiceCollection services)
        {
            // Adds services required for using options.
            services.AddOptions();
    
            // Register the IConfiguration instance which MyOptions binds against.
            services.Configure<MyOptions>(Configuration);
    
        }
    }

Then you could use it in your controllers in a way like presented below

    public class TestController : Controller
    {
        private readonly MyOptions _optionsAccessor;
    
        public TestController (IOptions<MyOptions> optionsAccessor)
        {
            _optionsAccessor = optionsAccessor.Value;
        }
    
        public IActionResult Index()
        {
            var key1 = _optionsAccessor.Key1 ;
            var key2 = _optionsAccessor.Key1 ;
            return Content($"option1 = {key1}, option2 = {key2}");
        }
    }

