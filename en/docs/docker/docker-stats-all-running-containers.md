---
title: "Docker stats all running containers"
slug: "docker-stats-all-running-containers"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Docker stats all running containers
    sudo docker stats $(sudo docker inspect -f "{{ .Name }}" $(sudo docker ps -q))

Shows live CPU usage of all running containers.

