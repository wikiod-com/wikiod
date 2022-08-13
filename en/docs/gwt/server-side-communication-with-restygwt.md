---
title: "Server side communication with RestyGwt"
slug: "server-side-communication-with-restygwt"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

As RestyGWT is a [GWT generator](http://www.gwtproject.org/doc/latest/DevGuideCodingBasicsDeferred.html#generators), it will automatically create the implementation of your client interface. Under the hood it will use the [RequestBuilder class](http://www.gwtproject.org/javadoc/latest/com/google/gwt/http/client/package-summary.html) to implement your calls. 

## Defining and using your REST client 
You should already have your backend REST resource available. On the client side (GWT) your need to

 1. Add RestyGwt dependency to your project with maven

        <dependency>
            <groupId>org.fusesource.restygwt</groupId>
            <artifactId>restygwt</artifactId>
            <version>2.2.0</version>
        </dependency>

 2. Add the inheritance to your module file

        <inherits name="org.fusesource.restygwt.RestyGWT"/>

 3. Create your client interface

        public interface PizzaService extends RestService {
            @POST
            @Path("pizzaorders")
            public void order(PizzaOrder request, 
                              MethodCallback<OrderConfirmation> callback);
        }

 4. Use your client where you want in you app

        PizzaService service = GWT.create(PizzaService.class);
        service.order(order, new MethodCallback<OrderConfirmation>() {
            
        public void onSuccess(Method method, OrderConfirmation response) {
          //code your stuff here
        }
        
        public void onFailure(Method method, Throwable exception) {
          //code your stuff here
        });

## Tweaking default config
Default config is configured in `import org.fusesource.restygwt.client.Defaults;` 

1. Service root. By default, RestyGWT will use module name to get rest service root, to change it, call on module load:  

       Defaults.setServiceRoot("/rest/");

 2. Date format. By default, RestyGWT will send unix timestamp as long for `java.util.Date`. If you don't want to do date processing on client side and just send a string, use

        Defaults.setDateFormat("yyyy-MM-dd'T'HH:mm");


    


