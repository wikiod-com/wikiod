---
title: "Using Guice with Jersey 2 (JAX-RS RI)"
slug: "using-guice-with-jersey-2-jax-rs-ri"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Getting Guice injections on JAX-RS resources
You will need the [guice-bridge](https://hk2.java.net/guice-bridge/) in your project.

    @ApplicationPath("api")
    public class ApiRest extends ResourceConfig {  
        @Inject
        public ApiRest(ServiceLocator serviceLocator, ServletContext servletContext) {
            packages("net.sargue.app.api");
    
            GuiceBridge.getGuiceBridge().initializeGuiceBridge(serviceLocator);
            GuiceIntoHK2Bridge guiceBridge = serviceLocator.getService(GuiceIntoHK2Bridge.class);
            Injector injector = (Injector) servletContext.getAttribute(Injector.class.getName());
            if (injector == null)
                throw new RuntimeException("Guice Injector not found");
            guiceBridge.bridgeGuiceInjector(injector);
    
            register(RolesAllowedDynamicFeature.class);
        }
    }

