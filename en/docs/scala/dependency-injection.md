---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Cake Pattern with inner implementation class.
    //create a component that will be injected
    trait TimeUtil {
        lazy val timeUtil = new TimeUtilImpl()
    
        class TimeUtilImpl{
            def now() = new DateTime()
        }
    }
    
    //main controller is depended on time util
    trait MainController {
        _ : TimeUtil => //inject time util into main controller

        lazy val mainController = new MainControllerImpl()
        
        class MainControllerImpl {
            def printCurrentTime() = println(timeUtil.now()) //timeUtil is injected from TimeUtil trait
        }
    }

    object MainApp extends App {
        object app extends MainController 
           with TimeUtil //wire all components

        app.mainController.printCurrentTime()
    }

In the above example, I demonstrated how to inject TimeUtil into MainController.

The most important syntax is the self-annotation (`_: TimeUtil => `) which is to inject `TimeUtil` into `MainController`. In another word, `MainController` depends on `TimeUtil`.

I use inner class (e.g. `TimeUtilImpl`) in each component because, in my opinion, that it is easier for testing as we can mock the inner class. And it is also easier for tracing where the method is called from when project grows more complex.

Lastly, I wire all component together. If you are familiar with Guice, this is equivalent to `Binding`

