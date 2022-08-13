---
title: "Getting started with web development"
slug: "getting-started-with-web-development"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Create new Ring application with http-kit
[Ring](https://github.com/ring-clojure/ring) is de-facto standard API for clojure HTTP applications, similar to Ruby's Rack and Python's WSGI.

We're going to use it with [http-kit](http://www.http-kit.org/) webserver.

Create new Leiningen project:

    lein new app myapp

Add http-kit dependency to `project.clj`:

      :dependencies [[org.clojure/clojure "1.8.0"]
                     [http-kit "2.1.18"]]

Add `:require` for http-kit to `core.clj`:

    (ns test.core
      (:gen-class)
      (:require [org.httpkit.server :refer [run-server]]))

      
Define ring request handler. Request handlers are just functions from request to response and response is just a map:

    (defn app [req]
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    "hello HTTP!"})

Here we just return 200 OK with the same content for any request.

Start the server in `-main` function:

    (defn -main
      [& args]
      (run-server app {:port 8080}))

Run with `lein run` and open `http://localhost:8080/` in browser.

## New web application with Luminus
[Luminus](http://www.luminusweb.net) is a Clojure micro-framework based on a set of lightweight libraries. It aims to provide a robust, scalable, and easy to use platform. With Luminus you can focus on developing your app the way you want without any distractions. It also has very good documentation that covers some of the majour topics

It is very easy to start with luminus. Just create a new project with the following commands:

    lein new luminus my-app
    cd my-app
    lein run
Your server will start on the port 3000

Running `lein new luminus myapp` will create an application using the default profile template. However, if you would like to attach further functionality to your template you can append profile hints for the extended functionality.

## Web Servers ##

 - +aleph - adds Aleph server support to the project
 - +jetty - adds Jetty support to the project
 - +http-kit - adds the HTTP Kit web server to the project
## databases ##
 - +h2 - adds db.core namespace and H2 db dependencies
 - +sqlite - adds db.core namespace and SQLite db dependencies
 - +postgres - adds db.core namespace and add PostreSQL dependencies
 - +mysql - adds db.core namespace and add MySQL dependencies
 - +mongodb - adds db.core namespace and MongoDB dependencies
 - +datomic - adds db.core namespace and Datomic dependencies

## miscellaneous ## 

 - +auth - adds Buddy dependency and authentication middleware
 - +auth-jwe - adds Buddy dependency with the JWE backend
 - +cider - adds support for CIDER using CIDER nREPL plugin
 - +cljs - adds [ClojureScript][cljs] support with [Reagent](https://reagent-project.github.io/)
 - +re-frame - adds [ClojureScript][cljs] support with [re-frame](https://github.com/Day8/re-frame)
 - +cucumber - a profile for cucumber with clj-webdriver
 - +swagger - adds support for Swagger-UI using the compojure-api library
 - +sassc - adds support for SASS/SCSS files using SassC command line compiler
 - +service - create a service application without the front-end boilerplate such as HTML templates
 - +war - add support of building WAR archives for deployment to servers such as Apache Tomcat (should NOT be used for Immutant apps running on WildFly)
 - +site - creates template for site using the specified database (H2 by default) and ClojureScript

To add a profile simply pass it as an argument after your application name, eg:

    lein new luminus myapp +cljs

You can also mix multiple profiles when creating the application, eg:

    lein new luminus myapp +cljs +swagger +postgres

