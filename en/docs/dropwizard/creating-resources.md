---
title: "Creating Resources"
slug: "creating-resources"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## GET requests
    @Path("/hello")
    public class HelloResource {

        /**
         * A request to /hello would get the response "Hello World"
         */
        @GET
        public String exampleGet() {
            return "Hello World";
        }
        
        /**
         * A request to /hello/bob would get the response "Hello bob"
         */
        @Path("{name}")
        @GET
        public String exampleWithParameter(@PathParam("name") String name) {
            return "Hello " + name;
        }


    }



## Custom responses
    import javax.ws.rs.*;
    import javax.ws.rs.core.Response;
    import java.util.HashMap;
    import java.util.Map;
    import java.util.Objects;
    
    @Path("/alphabet/{letter}")
    public class AlphabetResource {
    
        private final Map<String, String> alphabet;
        
        public AlphabetResource() {
            this.alphabet = new HashMap<>();
            this.alphabet.put("A", "Apple");
        }
        
        @GET
        public Response get(@PathParam("letter") String letter) {
            if (alphabet.containsKey(letter)) {
                return Response.ok(alphabet.get(letter)).build();
            } else {
                return Response.status(Response.Status.NOT_FOUND).build();
            }
        }
        
        @PUT
        public Response put(@PathParam("letter") String letter, String value) {
            if (alphabet.containsKey(letter)) {
                return Response.status(Response.Status.CONFLICT).build();
            } else {
                alphabet.put(letter, value);
                return Response.noContent().build();
            }
        }
        
        @POST
        public Response post(@PathParam("letter") String letter, String value) {
            if (alphabet.containsKey(letter) && Objects.equals(alphabet.get(letter), value)) {
                return Response.notModified().build();
            } else {
                alphabet.put(letter, value);
                return Response.noContent().build();
            }
        }
    }
        
        

These can be called with the following curl commands

    curl -v -X GET http://localhost:8080/alphabet/A
    curl -v -X PUT http://localhost:8080/alphabet/A -d "Avacado"
    curl -v -X PUT http://localhost:8080/alphabet/B -d "Banana"
    curl -v -X POST http://localhost:8080/alphabet/A -d "Apple"
    curl -v -X POST http://localhost:8080/alphabet/A -d "Avacado"



