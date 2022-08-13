---
title: "API Gateway"
slug: "api-gateway"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Microservices architecture offers great flexibility to decouple the applications and develop independent applications. A Microservice should always be independently testable & deployable.

But, as you keep on having too many services, there is a **need to have an API Gateway**. 

You can't expose all your services to external clients. You need to have some layer of abstraction which acts as a gatekeeper for all your Microservices. One entry point for all your services.

## Overview
Suppose you have an E-commerce cloud having various Microservices as shopping cart service, Order service, Inventory service and so on. You need to have an API gateway as an Edge service to the outer world. 

[![API Gateway][1]][1]

The API gateway abstracts the details(host & port) about the underline Microservices. Now, all your clients just need to know one server URL which is your API gateway. Any changes in any other Miroservice would not require any changes in your client app. Whenever an API gateway gets a request, **it routes the request to a specific Microservice**.



  [1]: https://i.stack.imgur.com/EdU03.png

