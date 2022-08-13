---
title: "Dependency Injection - Scala"
slug: "dependency-injection---scala"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
- class MyClassUsingAnother @Inject() (myOtherClassInjected: MyOtherClass) { (...) }
- @Singleton class MyClassThatShouldBeASingleton (...)

## Basic usage
<!-- language-all: lang-scala -->
A typical singleton class :

    import javax.inject._
    @Singleton
    class BurgersRepository {
        // implementation goes here
    }

Another class, requiring access to the first one.

    import javax.inject._
    class FastFoodService @Inject() (burgersRepository: BurgersRepository){
        // implementation goes here
        // burgersRepository can be used
    }

Finally a controller using the last one. Note since we didn't mark the FastFoodService as a singleton, a new instance of it is created each time it is injected.

    import javax.inject._
    import play.api.mvc._
    @Singleton
    class EatingController @Inject() (fastFoodService: FastFoodService) extends Controller {
        // implementation goes here
        // fastFoodService can be used
    }

## Defining custom bindings in a Module
<!-- language-all: lang-scala -->
Basic usage of dependency injection is done by the annotations. When you need to tweak things a little bit, you need custom code to further specify how you want some classes to be instantiated and injected. This code goes in what is called a Module.
    
    import com.google.inject.AbstractModule
    // Play will automatically use any class called `Module` that is in the root package
    class Module extends AbstractModule {
    
      override def configure() = {
        // Here you can put your customisation code.
        // The annotations are still used, but you can override or complete them.
        
        // Bind a class to a manual instantiation of it
        // i.e. the FunkService needs not to have any annotation, but can still
        // be injected in other classes
        bind(classOf[FunkService]).toInstance(new FunkService)

        // Bind an interface to a class implementing it
        // i.e. the DiscoService interface can be injected into another class
        // the DiscoServiceImplementation is the concrete class that will
        // be actually injected.
        bind(classOf[DiscoService]).to(classOf[DiscoServiceImplementation])

        // Bind a class to itself, but instantiates it when the application starts
        // Useful to executes code on startup
        bind(classOf[HouseMusicService]).asEagerSingleton()
      }
    
    }

## Injecting Play classes
<!-- language-all: lang-scala -->
You will often need to access instances of classes from the framework itself (like the WSClient, or the Configuration). You can inject them in your own classes :

    class ComplexService @Inject()(
      configuration: Configuration,
      wsClient: WSClient,
      applicationLifecycle: ApplicationLifecycle,
      cacheApi: CacheApi,
      actorSystem: ActorSystem,
      executionContext: ExecutionContext
      ) {
      // Implementation goes here
      // you can use all the injected classes :
      //
      // configuration to read your .conf files
      // wsClient to make HTTP requests
      // applicationLifecycle to register stuff to do when the app shutdowns
      // cacheApi to use a cache system
      // actorSystem to use AKKA
      // executionContext to work with Futures
    }

Some, like the ExecutionContext, will likely more easy to use if they're imported as implicit. Just add them in a second parameter list in the constructor :

    class ComplexService @Inject()(
      configuration: Configuration,
      wsClient: WSClient
      )(implicit executionContext: ExecutionContext) {
      // Implementation goes here
      // you can still use the injected classes
      // and executionContext is imported as an implicit argument for the whole class
    }

