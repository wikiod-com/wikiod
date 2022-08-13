---
title: "Dockerfile contents ordering"
slug: "dockerfile-contents-ordering"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

1. Base image declaration (`FROM`)
2. Metadata (e.g. `MAINTAINER`, `LABEL`)
2. Installing system dependencies (e.g. `apt-get install`, `apk add`)
3. Copying app dependencies file (e.g. `bower.json`, `package.json`, `build.gradle`, `requirements.txt`)
4. Installing app dependencies (e.g. `npm install`, `pip install`)
5. Copying entire code base
6. Setting up default runtime configs (e.g. `CMD`, `ENTRYPOINT`, `ENV`, `EXPOSE`)

These orderings are made for optimizing build time using Docker's built-in caching mechanism.

Rule of thumbs:

> Parts that change often (e.g. codebase) should be placed near bottom of Dockerfile, and vice-versa. Parts that rarely change (e.g. dependencies) should be placed at top.


## Simple Dockerfile
```Dockerfile
# Base image
FROM python:2.7-alpine

# Metadata
MAINTAINER John Doe <johndoe@example.com>

# System-level dependencies
RUN apk add --update \
    ca-certificates \
    && update-ca-certificates \
    && rm -rf /var/cache/apk/*

# App dependencies
COPY requirements.txt /requirements.txt
RUN pip install -r /requirements.txt

# App codebase
WORKDIR /app
COPY . ./

# Configs
ENV DEBUG true
EXPOSE 5000
CMD ["python", "app.py"]
```


MAINTAINER will be deprecated in Docker 1.13, and should be replaced by using LABEL. ([Source][1])

Example:
LABEL Maintainer="John Doe <johndoe@example.com>"


  [1]: https://github.com/docker/docker/blob/master/docs/deprecated.md#maintainer-in-dockerfile

