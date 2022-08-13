---
title: "Running jetty as embedded container"
slug: "running-jetty-as-embedded-container"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Maven Jetty plugin
This example applies to maven projects. Add the *jetty-maven-plugin* to  build element as below.

    <build>
       <plugins>
          <plugin>
             <groupId>org.eclipse.jetty</groupId>
             <artifactId>jetty-maven-plugin</artifactId>
             <version>9.4.0.M0</version>
          </plugin>
       </plugins>
    </build>

Execute *mvn jetty:run*

It downloads necessary dependencies and starts the jetty server. Console shows below lines if the server has started-

    [INFO] Started ServerConnector@7a31eb5d{HTTP/1.1,[http/1.1]}{0.0.0.0:8080}
    [INFO] Started @10535ms
    [INFO] Started Jetty Server

Application can be accessed in *http://localhost:8080/<app name>*

If you want to start jetty on different port-

Using system property during launch

    mvn -Djetty.http.port=8181 jetty:run

Or permanently add to plugin configurations

    <plugin>
      <groupId>org.eclipse.jetty</groupId>
      <artifactId>jetty-maven-plugin</artifactId>
      <version>9.4.0.M0</version>
      <configuration>
         <httpConnector>
           <port>8181</port>
         </httpConnector>
      </configuration>
    </plugin>


