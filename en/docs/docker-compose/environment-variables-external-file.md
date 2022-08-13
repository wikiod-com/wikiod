---
title: "Environment variables external file"
slug: "environment-variables-external-file"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

There a number of ways to include environment variables into the docker application. Here are some examples:

## Via External File
docker-composer.yml
```
  web:
    ...
    env_file:
     - ./filename
```

filename
```
variable=value
```

## within the docker-compose itself
    app:
      ...
      environment:
        - var=value

