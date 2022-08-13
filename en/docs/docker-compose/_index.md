---
title : docker-compose Tutorial
slug : docker-compose-tutorial
weight : 9909
draft : false
images : []
type : docs
---

Compose is a tool for defining and running multi-container Docker applications. With Compose, you use a Compose file to configure your application’s services. Then, using a single command, you create and start all the services from your configuration. To learn more about all the features of Compose see the list of features.

Using Compose is basically a three-step process.

1. Define your app’s environment with a `Dockerfile` so it can be reproduced anywhere.
2. Define the services that make up your app in `docker-compose.yml` so they can be run together in an isolated environment.
3. Lastly, run `docker-compose up` and Compose will start and run your entire app.

