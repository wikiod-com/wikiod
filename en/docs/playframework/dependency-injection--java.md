---
title: "Dependency injection - Java"
slug: "dependency-injection---java"
draft: false
images: []
weight: 9917
type: docs
toc: true
---

## Dependency injection with Guice - Play 2.4, 2.5
Guice is the default dependency injection (further <b>DI</b>) framework of Play. Other frameworks may be used as well, but using Guice makes development efforts easier, since Play takes care for things under the veil.

Injection of Play API-s
=======================

Starting from Play 2.5 several API-s, which were static in the earlier versions, should be created with <b>DI</b>. These are, for example, *Configuration*, *JPAApi*, *CacheApi*, etc.
 
Injecting method of Play API-s is different for a class, which is automatically injected by Play and for a custom class.
Injection in an **automatically injected** class is just as simple as putting appropriate *@Inject* annotation on either field or constructor. For example, to inject *Configuration* in a controller with property injection:

    @Inject 
    private Configuration configuration;
or with constructor injection:

    private Configuration configuration;
    @Inject
    public MyController(Configuration configuration) {
        this.configuration = configuration;
    }
Injection in a **custom** class, which is registered for <b>DI</b>, should be done just like it is done for automatically injected class - with *@Inject* annotation.

Injection from a **custom** class, which is not bound for <b>DI</b>, should be done by explicit call to an injector with *Play.current().injector()*. For example, to inject *Configuration* in a custom class define a configuration data member like this:

    private Configuration configuration = Play.current().injector().instanceOf(Configuration.class);

Custom injection binding
========================

Custom injection binding may be done with **@ImplementedBy** annotation or in a programmatic way with **Guice module**.

Injection with @ImplementedBy annotation
----------------------------------------
Injection with @ImplementedBy annotation is the simplest way. 
The example below shows a service, which provides a facade to **cache**. 
1. The service is defined by an interface *CacheProvider* as following:
    
       @ImplementedBy(RunTimeCacheProvider.class)
       public interface CacheProvider {  
          CacheApi getCache();  
       }  
2. The service is implemented by a class RunTimeCacheProvider:

       public class RunTimeCacheProvider implements CacheProvider {  
          @Inject  
          private CacheApi appCache;  
          @Override  
          public public CacheApi getCache() {  
            return appCache;  
          }  
       }  
**Note**: the *appCache* data member is injected upon creation of a *RunTimeCacheProvider* instance.
3. Cache inspector is defined as a member of a controller with *@Inject* annotation and is called from the controller:

    
    public class HomeController extends Controller {
      @Inject
      private CacheProvider cacheProvider;
      ...      
      public Result getCacheData() {
            Object cacheData = cacheProvider.getCache().get("DEMO-KEY");
            return ok(String.format("Cache content:%s", cacheData));  
      }      
Injection with *@ImplementedBy* annotation creates the fixed binding: *CacheProvider* in the above example is always instantiated with *RunTimeCacheProvider*. Such method fits only for a case, when there is an interface with a single implementation. It cannot help for an interface with several implementations or a class implemented as a singleton without abstract interface. Honestly speaking, @ImplementedBy will be used in rare cases if it all. It is more likely to use programmatic binding with <b>Guice module</b>.

Injection binding with a default Play module
--------------------------------------------
The default Play module is a class named *Module* in the root project directory defined like this: 

    import com.google.inject.AbstractModule;  
    public class Module extends AbstractModule {  
      @Override  
      protected void configure() {  
          // bindings are here           
      }  
    }  
**Note**: The snippet above shows binding inside configure, but of course any other binding method will be respected.

For programmatic binding of *CacheProvider* to *RunTimeCacheProvider*:
1. Remove *@ImplementedBy* annotation from the definition of *CacheProvider*:

    
    public interface CacheProvider {  
      CacheApi getCache();  
    }  
2. Implement Module *configure* as following:


    public class Module extends AbstractModule {  
      @Override  
      protected void configure() {  
        bind(CacheProvider.class).to(RunTimeCacheProvider.class);
      }  
    }  


Flexible injection binding with a default Play module
-----------------------------------------------------
*RunTimeCacheProvider* does not work well in *JUnit* tests with fake application (see unit tests topic). So, the different implementation of *CacheProvider* is demanded for unit tests. Injection binding should be done according to the environment. 

Let's see an example.
1. The class *FakeCache* provides a stub implementation of *CacheApi* to be used while running tests (its implementation is not such interesting - it is just a map).
2. The class *FakeCacheProvider* implements *CacheProvider* to be used while running tests:


    public class FakeCacheProvider implements CacheProvider {  
      private final CacheApi fakeCache = new FakeCache(); 
      @Override  
      public CacheApi getCache() {  
        return fakeCache;
      }  
    }  
2. Module is implemented as following:


    public class Module extends AbstractModule { 
      private final Environment environment;
      public Module(Environment environment, Configuration configuration) {
        this.environment = environment;
      }
      @Override  
      protected void configure() {  
         if (environment.isTest() ) {
          bind(CacheProvider.class).to(FakeCacheProvider.class);
        }
        else {
          bind(CacheProvider.class).to(RuntimeCacheProvider.class);   
        }
      }  
    }  
The example is good only for educational purpose. Binding for tests inside the module is not the best practice, since this couples between application and tests. Binding for tests should be done rather by tests itself and module should not be aware on test-specific implementation. See how to do this better in ... . 

Injection binding with a custom module
--------------------------------------
A custom module is very similar to the default Play module. The difference is that it may have whatever name and belong to whatever package. For example, a module OnStartupModule belongs to the package modules.

    package modules;  
    import com.google.inject.AbstractModule;  
    public class OnStartupModule extends AbstractModule {  
       @Override  
       protected void configure() {  
           ...            
       }  
    }  
A custom module should be explicitly enabled for invocation by Play. For the module *OnStartupModule* the following should be added into *application.conf*:

    play.modules.enabled += "modules.OnStartupModule"


