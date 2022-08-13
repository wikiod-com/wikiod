---
title: ".NET Core with Docker"
slug: "net-core-with-docker"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Fill with examples of using Docker on .NET Core platform, official base images for .NET Core application and self-hosted .NET Core app as well

## Dockerfile sample
.NET Core app should be published using `dotnet publish`

    FROM microsoft/dotnet:latest
    COPY bin/Debug/netcoreapp1.0/publish/ /root/
    EXPOSE 5000
    ENTRYPOINT dotnet /root/sampleapp.dll

