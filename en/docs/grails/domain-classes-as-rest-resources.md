---
title: "Domain classes as REST resources"
slug: "domain-classes-as-rest-resources"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The easiest way to create a RESTful API in Grails is to expose a domain class as a REST resource. This can be done by adding the grails.rest.Resource transformation to any domain class.

## Simple REST API with grails
    import grails.rest.*
    
    @Resource(uri='/books')
    class Book {
    
        String title
    
        static constraints = {
            title blank:false
        }
    }


Simply by adding the Resource transformation and specifying a URI, your domain class will automatically be available as a REST resource in either XML or JSON formats. The transformation will automatically register the necessary RESTful URL mapping and create a controller called BookController.

You can try it out by adding some test data to BootStrap.groovy:

    def init = { servletContext ->
        new Book(title:"The Stand").save()
        new Book(title:"The Shining").save()
    }


And then hitting the URL `http://localhost:8080/books/1`, which will render the response like:

    <?xml version="1.0" encoding="UTF-8"?>
    <book id="1">
        <title>The Stand</title>
    </book>

If you change the URL to `http://localhost:8080/books/1.json` you will get a JSON response such as:
 

    {"id":1,"title":"The Stand"}

If you wish to change the default to return JSON instead of XML, you can do this by setting the formats attribute of the Resource transformation:

    import grails.rest.*
    
    @Resource(uri='/books', formats=['json', 'xml'])
    class Book {
        ...
    }

## Mapping to REST resources
If you prefer to keep the declaration of the URL mapping in your UrlMappings.groovy file then simply removing the uri attribute of the Resource transformation and adding the following line to `UrlMappings.groovy` will suffice:

    "/books"(resources:"book")

Extending your API to include more end points then becomes trivial:

    "/books"(resources:"book") {
        "/publisher"(controller:"publisher", method:"GET")
    }

The above example will expose the URI `/books/1/publisher.`

## Add HTTPS to Grails Server
SSL Certificates use something called public key cryptography.We need to use Https instead of Http because of keeping data secure between servers and improving customer trust. To enable this option in grails, we have to run our app differently. The command below: 

    grails run-app -https

