---
title: "Webservice usage with play WSClient"
slug: "webservice-usage-with-play-wsclient"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Link to official documentation: https://www.playframework.com/documentation/2.5.x/ScalaWS

## Basic usage (Scala)
<!-- language-all: lang-scala -->
HTTP requests are made through the WSClient class, which you can use as an injected parameter into your own classes.


    import javax.inject.Inject

    import play.api.libs.ws.WSClient
    
    import scala.concurrent.{ExecutionContext, Future}
    
    class MyClass @Inject() (
      wsClient: WSClient
    )(implicit ec: ExecutionContext){
      
      def doGetRequest(): Future[String] = {
        wsClient
          .url("http://www.google.com")
          .get()
          .map { response =>
          // Play won't check the response status,
          // you have to do it manually
          if ((200 to 299).contains(response.status)) {
            println("We got a good response")
            // response.body returns the raw string
            // response.json could be used if you know the response is JSON
            response.body
          } else
            throw new IllegalStateException(s"We received status ${response.status}")
        }
      }
    }

