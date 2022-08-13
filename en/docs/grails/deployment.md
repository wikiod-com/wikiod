---
title: "Deployment"
slug: "deployment"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Executable Jar
One of the easiest ways to deploy Grails 3.x is to build an executable jar file that embeds a servlet container (Tomcat, Undertow, etc) with the application.

Modify `build.gradle`:

```
// Remove or comment out the war plugin:
// apply plugin:"war"

// Enable the executable jar:
springBoot {
    executable = true
}

// Optional: Customize the jar properties:
//  https://docs.gradle.org/current/dsl/org.gradle.api.tasks.bundling.Jar.html
jar {
    archiveName('myapp.jar')
}
```

Build using `./gradlew assemble`

Resulting jar is now a fully executable app you can startup:

```
$ head build/libs/myapp.jar
#!/bin/bash
#
#    .   ____          _            __ _ _
#   /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
#  ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
#   \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
#    '  |____| .__|_| |_|_| |_\__, | / / / /
#   =========|_|==============|___/=/_/_/_/
#   :: Spring Boot Startup Script ::
#
```

You can start it as you would normally for any command line app:
```bash
$ ./build/libs/myapp.jar
Grails application running at http://localhost:8080 in environment: production
```

It also behaves like an init service:
```
$ ln -s /opt/myapp/myapp.jar /etc/init.d/myapp
$ service myapp [start|stop|status|restart]
```

Detailed documentation is under the spring-boot docs: http://docs.spring.io/spring-boot/docs/current/reference/html/deployment-install.html

## War File Creation
When we write an Web application in Grails, to deploy the application we need a "war" file that need's to be put in the servlet container (Tomcat etc).

First goto the project directory : 

     cd to_project_directory

 1. War file creation from command prompt : 


    grails war
 2.Its always recommendable that you clean out your application before war creation
    
Cleaning application from command prompt : 

    grails clean

Combining the above two steps in one will result in

    grails clean && grails war

Also you can specify the environment in which you want to create the war file.

    grails [environment] war

Where `[environment]` can take the following values: `dev`, `prod` or `test` for example.

Unlike other commands, the war command runs in the production environment by default instead of development.


 

 

