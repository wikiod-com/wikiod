---
title: "REST API"
slug: "rest-api"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

The WordPress REST API provides API endpoints for WordPress data types that allow developers to interact with sites remotely by sending and receiving JSON (JavaScript Object Notation) objects.

When you send content to or make a request to the API, the response will be returned in JSON. This enables developers to create, read and update WordPress content from client-side JavaScript or from external applications, even those written in languages beyond PHP.


To get this WordPress REST API simple example to work for you, you need to learn how it works in more detail. Official documentation recommends learning about:

 1. Routes/Endpoints - which are mappings of individual HTTP methods to routes known as  "endpoints" - you do it using [register_rest_route() function](https://developer.wordpress.org/reference/functions/register_rest_route/), and here you can find more about [Routes and Endpoints](https://developer.wordpress.org/rest-api/extending-the-rest-api/routes-and-endpoints/).

 2. Requests - WordPress REST API defines `WP_REST_Request` class which is used to store and retrieve information for the current request. `WP_REST_Request` objects are automatically generated for you whenever you make an HTTP request to a registered route. The data specified in the request will determine what response you get back out of the API. Here can learn more about the [WP_REST_Request class](https://developer.wordpress.org/reference/classes/wp_rest_request/).

 3. Responses - are the data you get back from the API. The `WP_REST_Response` provides a way to interact with the response data returned by endpoints. In your endpoint definition you name the callback (response) function to serve your interaction.
Here can learn more about the [WP_REST_Response class](https://developer.wordpress.org/reference/classes/wp_rest_response/).

 4. Schema - Each endpoint requires and provides slightly different data structures, and those structures are defined in the API Schema. If you want maintainable, discoverable, and easily extensible endpoints it is recommended to use the schema. Here you can learn more about the [Schema](https://developer.wordpress.org/rest-api/extending-the-rest-api/schema/).

 5. Controller Classes - they bring all elements together in a single place. With a controller class you can manage the registration of routes & endpoints, handle requests, utilize schema, and generate API responses. You have already learned about two controller classes: `WP_REST_Request` and `WP_REST_Response`.  Here you can learn more about the [Controller Classes](https://developer.wordpress.org/rest-api/extending-the-rest-api/controller-classes/)

Note: Some of this information is taken from the official [Wordpress REST APi Handbook](https://developer.wordpress.org/rest-api/)

## Complete working example
    add_action('rest_api_init', 'my_rest_validate_endpoint' );
    function my_rest_validate_endpoint() {

        // Declare our namespace
        $namespace = 'myrest/v1';

        // Register the route
        // Example URL matching this route:
        // http://yourdomain/wp-json/myrest/v1/guides/tag=europe/price=29
        register_rest_route($namespace,
                            // Using regular expressions we can initially validate the input
                            '/guides/tag=(?P<tag>[a-zA-Z0-9-]+)/price=(?P<price>[0-9]+)',
                            // We can have multiple endpoints for one route
                            array(
                                array(
                                    'methods'   => 'GET',
                                    'callback'  => 'my_get_guides_handler'
                                )
                            ),
                            // We can register our schema callback
                            // 'schema' => 'my_get_guide_schema',
                    
            );

        // You can register another route here the same way

    }


    // The callback handler for the endpoint
    function my_get_guides_handler(WP_REST_Request $request) {

        // Get the parameters:
        $tag = $request->get_param('tag');
        $price = $request->get_param('price');

        // Do something with the parameters
        // for instance: get matching guides from the DB into an array - $results
        // ...
        
        // Prepare the response
        $message = "We've found " . count($results) . " guides ";
        $message .= "(searching for a tag: " . $tag . ", with a price tag: " . $price . ")";

        // The response gets automatically converted into a JSON format
        return new WP_REST_Response(
            array(
                'results' => $results,
                'message' => $message,
                'status' => 'OK'
            ),
            200 );

    }

