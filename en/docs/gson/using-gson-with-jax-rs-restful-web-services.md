---
title: "Using Gson with JAX-RS (RESTful web services)"
slug: "using-gson-with-jax-rs-restful-web-services"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## JAX-RS provider to use Gson
This is a custom JAX-RS `@Provider` to use Gson as the JSON parser. The example also shows how to use custom Java 8 date/time converters.

<!-- language: lang-java --> 

    @Provider
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public class JerseyServerGson  
            implements MessageBodyWriter<Object>, MessageBodyReader<Object>
    {
      @Override
      public boolean isReadable(Class<?> type,
                                Type genericType,
                                Annotation[] annotations,
                                MediaType mediaType)
      {
        return true;
      }
    
      @Override
      public Object readFrom(Class<Object> type,
                             Type genericType,
                             Annotation[] annotations,
                             MediaType mediaType,
                             MultivaluedMap<String, String> httpHeaders,
                             InputStream entityStream)
              throws IOException, WebApplicationException
      {
        try ( InputStreamReader input = 
                      new InputStreamReader(entityStream, "UTF-8") ) {
          Gson gson = getGson();
          return gson.fromJson(input, genericType);
        }
      }
    
      @NotNull
      private Gson getGson() {
        return new GsonBuilder()
                .registerTypeAdapter(LocalDateTime.class,
                                     new AdapterLocalDateTime().nullSafe())
                .registerTypeAdapter(LocalDate.class,
                                     new AdapterLocalDate().nullSafe())
                .setPrettyPrinting()
                .serializeNulls()
                .create();
      }
    
      @Override
      public boolean isWriteable(Class<?> type,
                                 Type genericType,
                                 Annotation[] annotations,
                                 MediaType mediaType)
      {
        return true;
      }
    
      @Override
      public long getSize(Object o,
                          Class<?> type,
                          Type genericType,
                          Annotation[] annotations,
                          MediaType mediaType)
      {
        // Deprecated and ignored in Jersey 2
        return -1;
      }
    
      @Override
      public void writeTo(Object o,
                          Class<?> type,
                          Type genericType,
                          Annotation[] annotations,
                          MediaType mediaType,
                          MultivaluedMap<String, Object> httpHeaders,
                          OutputStream entityStream)
              throws IOException, WebApplicationException
      {
        try ( OutputStreamWriter writer = 
                      new OutputStreamWriter(entityStream, "UTF-8") ) {
          getGson().toJson(o, genericType, writer);
        }
      }
    }

