---
title: "Iron Web Framework"
slug: "iron-web-framework"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

[Iron](http://ironframework.io/) is a popular web framework for Rust (based on the lower-level [Hyper](http://hyper.rs/) library) which promotes the idea of extensibility through *middleware*. Much of the functionality needed to create a useful website can be found in Iron's middleware rather than the library itself.

## Simple 'Hello' Server
This example sends a hard-coded response to the user when they send a server request.

    extern crate iron;
    
    use iron::prelude::*;
    use iron::status;

    // You can pass the handler as a function or a closure. In this
    // case, we've chosen a function for clarity.
    // Since we don't care about the request, we bind it to _.
    fn handler(_: &mut Request) -> IronResult<Response> {
        Ok(Response::with((status::Ok, "Hello, Stack Overflow")))
    }

    fn main() {
        Iron::new(handler).http("localhost:1337").expect("Server failed!")
    }

When creating a new `Iron` server in this example, `expect` to catch any errors with a more descriptive error message. In production applications, *handle the error* produced (see the [documentation for `http()`](http://ironframework.io/doc/iron/struct.Iron.html#method.http)).

## Simple Routing with Iron
This example will provide basic web routing using Iron.

To begin with, you will need to add the Iron dependency to your `Cargo.toml` file.

    [dependencies]
    iron = "0.4.*"

We will use Iron's own Router library. For simplicity, the Iron project provides this library as part of the Iron core library, removing any need to add it as a separate dependency. Next we reference both the Iron library and the Router library.

    extern crate iron;
    extern crate router;

Then we import the required objects to enable us to manage routing, and return a response to the user.
    
    use iron::{Iron, Request, Response, IronResult};
    use iron::status;
    use router::{Router};

In this example, we'll keep it simple by writing the routing logic within our `main()` function. Of course, as your application grows, you will want to separate out routing, logging, security concerns, and other areas of your web application. For now, this is a good starting point.
    
    fn main() {
        let mut router = Router::new();
        router.get("/", handler, "handler");
        router.get("/:query", query_handler, "query_handler");

Let's go over what we've achieve so far. Our program currently instantiates a new Iron `Router` object, and attaches two "handlers" to two types of URL request: the first (`"/"`) is the root of our domain, and the second (`"/:query"`) is any path under root. 

By using a semi-colon before the word "query", we're telling Iron to take this part of the URL path as a variable and pass it into our handler.

The next line of code is how we instantiate Iron, designating our own `router` object to manage our URL requests. The domain and port are hard-coded in this example for simplicity.
    
        Iron::new(router).http("localhost:3000").unwrap();

Next, we declare two inline functions that are our handlers, `handler` and `query_handler`. These are both used to demonstrate fixed URLs and variable URLs. 

In the second function we take the `"query"` variable from the URL held by the request object, and we send it back to the user as a response. 
    
        fn handler(_: &mut Request) -> IronResult<Response> {
            Ok(Response::with((status::Ok, "OK")))
        }
    
        fn query_handler(req: &mut Request) -> IronResult<Response> {
            let ref query = req.extensions.get::<Router>()
                .unwrap().find("query").unwrap_or("/");
            Ok(Response::with((status::Ok, *query)))
        }
    }

If we run this example, we will be able to view the result in the web browser at `localhost:3000`. The root of the domain should respond with `"OK"`, and anything beneath the root should repeat the path back.

*The next step from this example could be the separation of routing and the serving of static pages.*


  [1]: http://hyper.rs

## Installing Iron
Add this dependency to the `Cargo.toml` file:

    [dependencies]
    iron = "0.4.0"

Run `cargo build` and Cargo will download and install the specified version of Iron.

