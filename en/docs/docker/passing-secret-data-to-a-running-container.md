---
title: "passing secret data to a running container"
slug: "passing-secret-data-to-a-running-container"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## ways to pass secrets in a container
The not very secure way (because `docker inspect` will show it) is to pass an environment variable to 

    docker run

such as

`docker run -e password=abc`

or in a file

`docker run --env-file myfile` 

where myfile can contain

`password1=abc
password2=def`

it is also possible to put them in a volume 

`docker run -v $(pwd)/my-secret-file:/secret-file`

some better ways, use

keywhiz
https://square.github.io/keywhiz/

vault
https://www.hashicorp.com/blog/vault.html

etcd with crypt
https://xordataexchange.github.io/crypt/

