---
title: "Dockerization of ASP.NET Application"
slug: "dockerization-of-aspnet-application"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Dockerization
It is nessecary to have .NET or a mono-aspnet package. 

It is important to understand the importance of dockerization. Install dotnet on ubuntu or the OS you are working on.

Installing DOTNET

    $ sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
    $ sudo apt-key adv --keyserver apt-mo.trafficmanager.net --recv-keys 417A0893
    $ sudo apt-get update
    
    Ubuntu 16.04

 

    $ sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ xenial main" > /etc/apt/sources.list.d/dotnetdev.list'
    $ sudo apt-key adv --keyserver apt-mo.trafficmanager.net --recv-keys 417A0893
    $ sudo apt-get update

Install .NET Core SDK

 

    $ sudo apt-get install dotnet-dev-1.0.0-preview2-003121

COURTESY : https://www.microsoft.com/net/core#ubuntu

For installation of Docker follow, https://docs.docker.com/engine/installation/linux/ubuntulinux/

FOR PORT :

    Kestrel server port : 5000 
    Docker Deamon will listen to port :
    
     EXPOSE 5000/tcp

For building docker :

     $ sudo docker build -t myapp .

For running the docker container :

     $ sudo docker run -t -d -p 8195:5000 myapp

For visiting site :

 

    $ ifconfig 

    eth0 : ***.***.**  
     server-ip-address

Site will be available on (given this configuration.) :

     http://server-ip-address:8195
Docker Processes. It will list running processes. 
  

      $ sudo docker ps
 


To delete the process or the container. 
 

     $ sudo docker rm -rf <process_id>



## Dockerfile and Nuget
Dockerization of ASP.NET Application requires a Dockerfile for configuration and running it as a docker container.

    FROM microsoft/dotnet:latest
    
    RUN apt-get update && apt-get install sqlite3 libsqlite3-dev
    
    COPY . /app
    
    WORKDIR /app
    
    RUN ["dotnet", "restore"]
    
    RUN ["dotnet", "build"]
    
    RUN npm install && npm run postscript
    
    RUN bower install 
    
    RUN ["dotnet", "ef", "database", "update"]
    
    EXPOSE 5000/tcp
    
    ENTRYPOINT ["dotnet", "run", "--server.urls", "http://0.0.0.0:5000"]

A nuget feed configuration file helps in retrieving from the correct source. The usage of this file depends on the current configuration of the project and can change to suite project's requirement. 

     <?xml version="1.0" encoding="utf-8"?>
       <configuration>
        <packageSources>
         <add key="nuget.org" value="https://api.nuget.org/v3/index.json"  protocolVersion="3" />
        <packageSources>
        <packageRestore>
         <add key="enabled" value="True" />
         <add key="automatic" value="True" />
        <packageRestore>
       <bindingRedirects>
        <add key="skip" value="False" />
       </bindingRedirects>
     </configuration>

## POSTGRESQL Support.


    "Data": {
        "DefaultConnection": {
            "ConnectionString": "Host=localhost;Username=postgres;Password=******;Database=postgres;Port=5432;Pooling=true;"
        }
      },

