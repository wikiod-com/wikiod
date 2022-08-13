---
title: "Akka HTTP"
slug: "akka-http"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Akka HTTP is a light-weight HTTP server and client library, using akka-streams under the hood

## Akka HTTP server: Hello World (Scala DSL)
The following app will start an HTTP server listening on port 8080 that returns `Hello world` on `GET   /hello/world`

    import akka.actor.ActorSystem
    import akka.http.scaladsl.Http
    import akka.http.scaladsl.server.Directives._
    import akka.http.scaladsl.server._
    import akka.stream.ActorMaterializer
    
    import scala.concurrent.Await
    import scala.concurrent.duration.Duration
    
    object HelloWorld extends App {
    
      implicit val system = ActorSystem("ProxySystem")
      implicit val mat = ActorMaterializer()
    
      val route: Route = get {
        path("hello" / "world") {
          complete("Hello world")
        }
      }
    
      val bindingFuture = Http().bindAndHandle(Route.handlerFlow(route), "127.0.0.1", port = 8080)
    
      Await.result(system.whenTerminated, Duration.Inf)
    
    }

