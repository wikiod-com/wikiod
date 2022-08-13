---
title: "Docker events"
slug: "docker-events"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Launch a container and be notified of related events
The [documentation][1] for `docker events` provides details, but when debugging it may be useful to launch a container and be notified immediately of any related event: 

`docker run... & docker events --filter 'container=$(docker ps -lq)'`

In `docker ps -lq`, the `l` stands for `last`, and the `q` for `quiet`. This removes the `id` of the last container launched, and creates a notification immediately if the container dies or has another event occur.


  [1]: https://docs.docker.com/engine/reference/commandline/events/

