---
title: "Using Guice with Java WebSockets (JSR-356)"
slug: "using-guice-with-java-websockets-jsr-356"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Configuration to get Guice injection on Websocket endpoints
First, we need a custom endpoint builder.

    public class WSConfigurator extends ServerEndpointConfig.Configurator {  
      @Inject
      private static Injector injector;
    
      @Override
      public <T> T getEndpointInstance(Class<T> endpointClass)
              throws InstantiationException
      {
        return injector.getInstance(endpointClass);
      }
    }

We need to bootstrap the injector in the above configurator from one of our Guice modules.

    public class WebSocketModule extends AbstractModule {  
      @Override
      protected void configure() {
        requestStaticInjection(WSConfigurator.class);
      }
    }

Finally, we can use `@Inject` on the endpoints' constructor.

    @ServerEndpoint(
            value = "/ws/sync",

           configurator = WSConfigurator.class)
    public class WSSync extends AsyncWebSocketServer {  
      @Inject
      public WSSync(EventBus eventBus) {
        ...
      }
    }

