---
title: "docker inspect getting various fields for keyvalue and elements of list"
slug: "docker-inspect-getting-various-fields-for-keyvalue-and-elements-of-list"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## various docker inspect examples
I find that the examples in the `docker inspect` documentation seem magic, but do not explain much.

Docker inspect is important because it is the clean way to extract information from a running container
`docker inspect -f ... container_id`

 (or all running container)

`docker inspect -f ... $(docker ps -q)`

avoiding some unreliable

`docker command | grep or awk | tr or cut`

When you launch a `docker inspect` you can get the values from the "top-level" easily, with a basic syntax like, for a container running htop (from https://hub.docker.com/r/jess/htop/) with a pid ae1 

`docker inspect -f '{{.Created}}' ae1`

can show

`2016-07-14T17:44:14.159094456Z` 

or

`docker inspect -f '{{.Path}}' ae1`

can show

`htop`

Now if I extract a part of my `docker inspect`

I see

`        "State": {
            "Status": "running",
            "Running": true,
            "Paused": false,
            "Restarting": false,
            "OOMKilled": false,
            "Dead": false,
            "Pid": 4525,
            "ExitCode": 0,
            "Error": "",
            "StartedAt": "2016-07-14T17:44:14.406286293Z",
            "FinishedAt": "0001-01-01T00:00:00Z"
`
So I get a dictionary, as it has `{ ...}` and a lot of key:values

So the command

`docker inspect -f '{{.State}}' ae1`

will return a list, such as

`{running true false false false false 4525 0  2016-07-14T17:44:14.406286293Z 0001-01-01T00:00:00Z}`

I can get the value of State.Pid easily

`docker inspect -f '{{ .State.Pid }}' ae1`

I get 

`4525`

Sometimes docker inspect gives a list as it begins with `[` and ends with `]`

another example, with another container

`docker inspect -f ‘{{ .Config.Env }}’ 7a7`

gives

`[DISPLAY=:0 PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin LANG=fr_FR.UTF-8 LANGUAGE=fr_FR:en LC_ALL=fr_FR.UTF-8 DEBIAN_FRONTEND=noninteractive HOME=/home/gg WINEARCH=win32 WINEPREFIX=/home/gg/.wine_captvty]`

In order to get the first element of the list, we add index before the required field and 0 (as first element) after, so 

`docker inspect -f ‘{{ index ( .Config.Env) 0 }}’ 7a7`

gives

`DISPLAY=:0`

We get the next element with 1 instead of 0 using the same syntax

`docker inspect -f ‘{{ index ( .Config.Env) 1 }}’ 7a7`

gives

`PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin`

We can get the number of elements of this list

`docker inspect -f ‘{{ len .Config.Env }}’ 7a7`

gives

`9`

and we can get the last element of the list, the syntax is not easy

 `docker inspect -f “{{ index .Config.Cmd $[$(docker inspect –format ‘{{ len .Config.Cmd }}’ $CID)-1]}}” 7a7`

