---
title: "Inspecting a running container"
slug: "inspecting-a-running-container"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Syntax
- docker inspect [OPTIONS] CONTAINER|IMAGE [CONTAINER|IMAGE...]

## Get specific information from a container
You can get an specific information from a container by running: 

    docker inspect -f '<format>' <container>

For instance, you can get the Network Settings by running:

    docker inspect -f '{{ .NetworkSettings }}' <container>

You can also get just the IP address:

    docker inspect -f '{{ .NetworkSettings.IPAddress }}' <container>

The parameter -f means format and will receive a Go Template as input to format what is expected, but this won’t bring a beautiful return, so try:   

    docker inspect -f '{{ json .NetworkSettings }}' {{containerIdOrName}}

the json keyword will bring the return as a JSON. 

So to finish, a little tip is to use python in there to format the output JSON: 

    docker inspect -f '{{ json .NetworkSettings }}' <container> | python -mjson.tool

And voila, you can query anything on the docker inspect and make it look pretty in your terminal.

It's also possible to use a utility called "[jq](http://stedolan.github.io/jq/)" in order to help process `docker inspect` command output.

    docker inspect -f '{{ json .NetworkSettings }}' aa1 | jq [.Gateway]

The above command will return the following output:

    [
      "172.17.0.1"
    ]

This output is actually a list containing one element. Sometimes, `docker inspect` displays a list of several elements, and you may want to refer to a specific element. For example, if `Config.Env` contains several elements, you can refer to the first element of this list using `index`:

    docker inspect --format '{{ index (index .Config.Env) 0 }}' <container>

The first element is indexed at zero, which means that the second element of this list is at index `1`:

    docker inspect --format '{{ index (index .Config.Env) 1 }}' <container>

Using `len` it is possible to get the number of elements of the list:

    docker inspect --format ‘{{ len .Config.Env }}’ <container>

And using negative numbers, it's possible to refer to the last element of the list:

    docker inspect –format “{{ index .Config.Cmd $[$(docker inspect –format ‘{{ len .Config.Cmd }}’ <container>)-1]}}” <container>

Some `docker inspect` information comes as a dictionary of key:value, here is an extract of a 
`docker inspect`of a jess/spotify running container

`    "Config": {
            "Hostname": "8255f4804dde",
            "Domainname": "",
            "User": "spotify",
            "AttachStdin": false,
            "AttachStdout": false,
            "AttachStderr": false,
            "Tty": false,
            "OpenStdin": false,
            "StdinOnce": false,
            "Env": [
                "DISPLAY=unix:0",
                "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
                "HOME=/home/spotify"
            ],
            "Cmd": [
                "-stylesheet=/home/spotify/spotify-override.css"
            ],
            "Image": "jess/spotify",
            "Volumes": null,
            "WorkingDir": "/home/spotify",
            "Entrypoint": [
                "spotify"
            ],
            "OnBuild": null,
            "Labels": {}
        },
`

so I an get the values of the whole Config section

`docker inspect -f '{{.Config}}' 825`

`{8255f4804dde  spotify false false false map[]  false false false [DISPLAY=unix:0 PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin HOME=/home/spotify] [-stylesheet=/home/spotify/spotify-override.css] false jess/spotify map[] /home/spotify [spotify] false  [] map[] }`


but also a single field, like the value of Config.Image

`docker inspect -f '{{index (.Config) "Image" }}' 825`

`jess/spotify`

or Config.Cmd

`docker inspect -f '{{.Config.Cmd}}' 825`

`[-stylesheet=/home/spotify/spotify-override.css]`







## Get container information
To get all the information for a container you can run: 

    docker inspect <container>



## Printing specific informations


## Inspect an image


## Debugging the container logs using docker inspect


## Examining stdout/stderr of a running container
    docker logs --follow <containerid>

This tails the output of the running container. This is useful if you did not set up a logging driver on the docker daemon.

