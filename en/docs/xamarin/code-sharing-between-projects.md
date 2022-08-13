---
title: "Code Sharing Between Projects"
slug: "code-sharing-between-projects"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## The Bridge Pattern
The Bridge pattern is one of the most basic Inversion of Control design patterns. For Xamarin, this pattern is used to reference platform-dependent code from a platform-independent context. For example: using Android's AlertDialog from a Portable Class Library or Xamarin Forms. Neither of those contexts knows what an AlertDialog object is, so you must wrap it in a box for them to use. 

<!-- language: c# -->

    // Define a common interface for the behavior you want in your common project (Forms/Other PCL)
    public interface IPlatformReporter
    {
        string GetPlatform();
    }


    // In Android/iOS/Win implement the interface on a class
    public class DroidReporter : IPlatformReporter
    {
        public string GetPlatform()
        {
            return "Android";
        }
    }


    public class IosReporter : IPlatformReporter
    {
        public string GetPlatform()
        {
            return "iOS";
        }
    }


    // In your common project (Forms/Other PCL), create a common class to wrap the native implementations
    public class PlatformReporter : IPlatformReporter
    {
        // A function to get your native implemenation
        public static func<IPlatformReporter> GetReporter;

        // Your native implementation
        private IPlatformReporter _reporter;

        // Constructor accepts native class and stores it
        public PlatformReporter(IPlatformReporter reporter)
        {
            _reporter = GetReporter();
        }

        // Implement interface behavior by deferring to native class
        public string GetPlatform()
        {
            return _reporter.GetPlatform();
        }
    }


    // In your native code (probably MainActivity/AppDelegate), you just supply a function that returns your native implementation
    public class MainActivity : Activity
    {
        protected override void OnCreate(Bundle bundle)
        {
            base.OnCreate(bundle);
            SetContentView(Resource.Layout.activity_main);

            PlatformReporter.GetReporter = () => { return new DroidReporter(); };
        }
    }


    public partial class AppDelegate : UIApplicationDelegate
    {
        UIWindow window;

        public override bool FinishedLaunching(UIApplication app, NSDictionary options)
        {
            window = new UIWindow(UIScreen.MainScreen.Bounds);
            window.RootViewController = new UIViewController();
            window.MakeKeyAndVisible();

            PlatformReporter.GetReporter = () => { return new IosReporter(); };

            return true;
        }
    }


    // When you want to use your native implementation in your common code, just do as follows:
    public void SomeFuncWhoCares()
    {
        // Some code here...

        var reporter = new PlatformReporter();
        string platform = reporter.GetPlatform();

        // Some more code here...
    }

## The Service Locator Pattern
The Service Locator design pattern is very nearly dependency injection. Like the Bridge Pattern, this pattern can be used to reference platform-dependent code from a platform-independent context. Most interestingly, this pattern relies on the singleton pattern -- everything you put into the service locator will be a defacto singleton. 

<!-- language: c# -->

    // Define a service locator class in your common project
    public class ServiceLocator {
        // A dictionary to map common interfaces to native implementations
        private Dictionary<object, object> _services;

        // A static instance of our locator (this guy is a singleton) 
        private static ServiceLocator _instance;
        
        // A private constructor to enforce the singleton
        private ServiceLocator() {
            _services = new Dictionary<object, object>();
        }

        // A Singleton access method
        public static ServiceLocator GetInstance() {
            if(_instance == null) {
                _instance = new ServiceLocator();
            }
    
            return _instance;
        }

        // A method for native projects to register their native implementations against the common interfaces
        public static void Register(object type, object implementation) {
            _services?.Add(type, implementation);
        }

        // A method to get the implementation for a given interface
        public static T Resolve<T>() {
            try {
                return (T) _services[typeof(T)];
            } catch {
                throw new ApplicationException($"Failed to resolve type: {typeof(T).FullName}");
            }
        }


    //For each native implementation, you must create an interface, and the native classes implementing that interface
    public interface IA {
        int DoAThing();
    }


    public interface IB {
        bool IsMagnificent();
    }


    public class IosA : IA {
        public int DoAThing() {
            return 5;
        }
    }


    public class DroidA : IA {
        public int DoAThing() {
            return 42;
        }
    }


    // You get the idea... 


    // Then in your native initialization, you have to register your classes to their interfaces like so:
    public class MainActivity : Activity
    {
        protected override void OnCreate(Bundle bundle)
        {
            base.OnCreate(bundle);
            SetContentView(Resource.Layout.activity_main);
    
            var locator = ServiceLocator.GetInstance();
            locator.Register(typeof(IA), new DroidA());
            locator.Register(typeof(IB), new DroidB());
        }
    }
    
    
    public partial class AppDelegate : UIApplicationDelegate
    {
        UIWindow window;
    
        public override bool FinishedLaunching(UIApplication app, NSDictionary options)
        {
            window = new UIWindow(UIScreen.MainScreen.Bounds);
            window.RootViewController = new UIViewController();
            window.MakeKeyAndVisible();
    
            var locator = ServiceLocator.GetInstance();
            locator.Register(typeof(IA), new IosA());
            locator.Register(typeof(IB), new IosB());
    
            return true;
        }
    }
    

    // Finally, to use your native implementations from non-native code, do as follows:
    public void SomeMethodUsingNativeCodeFromNonNativeContext() {
         // Some boring code here


         // Grabbing our native implementations for the current platform
         var locator = ServiceLocator.GetInstance();
         IA myIA = locator.Resolve<IA>();
         IB myIB = locator.Resolve<IB>();


        // Method goes on to use our fancy native classes
    }

    

