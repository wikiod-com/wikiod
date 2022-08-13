---
title: "springfox"
slug: "springfox"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Override default response messages
Springfox defines a set default response messages that are applied to all API controllers by default. This includes e.g. `201 - Created` and `204 - No Content`, as well as several `40x` responses. There might be cases, in which the default response messages don't apply for your API. You have to build-in possibilities to deal with this:

 - You can turn of the default response message and define your own using the `@ApiResponses` annotation.
 - You can define your own response messages globally

Turn of default response messages
-------------------------------

    docket.useDefaultResponseMessages(false);
You can now set your individual response messages on a per-controller level. E.g.

     @ApiResponses(value = {
            @ApiResponse(code=400, message = "This is a bad request, please stick to the API description", response = RestApiExceptionModel.class),
            @ApiResponse(code=401, message = "Your request cannot be authorized.", response = RestApiExceptionModel.class)
     })

Set your own default response messages
--------------------------------------

    ModelRef errorModel = new ModelRef("RestApiExceptionModel");
    List<ResponseMessage> responseMessages = Arrays.asList(
            new ResponseMessageBuilder().code(401).message("Unauthorized").responseModel(errorModel).build(),
            new ResponseMessageBuilder().code(403).message("Forbidden").responseModel(errorModel).build(),
            new ResponseMessageBuilder().code(404).message("NotFound").responseModel(errorModel).build());
    
    docket.globalResponseMessage(RequestMethod.POST, responseMessages)
            .globalResponseMessage(RequestMethod.PUT, responseMessages)
            .globalResponseMessage(RequestMethod.GET, responseMessages)
            .globalResponseMessage(RequestMethod.DELETE, responseMessages);



## Setup springfox using swagger-ui in spring-boot
 1. Get springfox into your application by using Maven or Gradle
 2. Create a new Docket bean in your application and configure it
 3. Document your API according to your needs
 4. Launch your application and see your achieved results

#1 Getting springfox with Maven
-------------------------------
Add the dependencies for swagger2 and swagger-ui in your pom.xml

    <dependency>
        <groupId>io.springfox</groupId>
        <artifactId>springfox-swagger2</artifactId>
        <version>2.6.0</version>
    </dependency>
    <dependency>
        <groupId>io.springfox</groupId>
        <artifactId>springfox-swagger-ui</artifactId>
        <version>2.6.0</version>
    </dependency>

#2 Configure your application to use swagger
--------------------------------------------
Add the annotation `@EnableSwagger2` to your `@SpringBootApplication` annotated main class and create a swagger Docket bean within this (or any other) configuration class.

    @Bean
    public Docket api() {                
        return new Docket(DocumentationType.SWAGGER_2)          
          .select()                                       
          .apis(RequestHandlerSelectors.any())
          .paths(PathSelectors.any())                     
          .build();
    }
This configuration will generate an API documentation over all spring controllers within your application. If you need to limit the API documentation generation to certain controllers you can choose between various RequestHandlerSelectors. E.g. you can generate your API documentation based on your package structure using `RequestHandlerSelectors.basePackage("your.package.structure")` or based specific classes that you've annotated using `RequestHandlerSelectors.withClassAnnotation(Api.class)`.

#3 Document your API
--------------------
Use the annotations as described in the [documentation][1], to enhance your controller classes and methods with additional information. To describe the general information of your api, like general title, description or the version, make use of the ApiInfoBuilder() within your Docket bean.

Example for the metadata definition using ApiInfoBuilder:

    // Within your configuration class
    public static ApiInfo metadata(){
        return new ApiInfoBuilder()
                .title("Your Title")
                .description("Your Description")
                .version("1.x")
                .build();
    }

    // Within your method that definies the Docket bean...
    docket.apiInfo(metadata());

    
 


  [1]: https://github.com/swagger-api/swagger-core/wiki/Annotations-1.5.X

