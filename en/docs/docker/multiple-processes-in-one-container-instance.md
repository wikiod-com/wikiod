---
title: "Multiple processes in one container instance"
slug: "multiple-processes-in-one-container-instance"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Usually each container should hosts one process. In case you need multiple processes in one container (e.g. an SSH server to login to your running container instance) you could get the idea to write you own shell script that starts those processes. In that case you had to take care about the ```SIGNAL``` handling yourself (e.g. redirecting a caught ```SIGINT``` to the child processes of your script). That's not really what you want. A simple solution is to use ```supervisord``` as the containers root process which takes care about ```SIGNAL``` handling and its child processes lifetime.

But keep in mind, that this ist not the "docker way". To achive this example in the docker way you would log into the `docker host` (the machine the container runs on) and run `docker exec -it container_name /bin/bahs`. This command opens you a shell inside the container as ssh would do.

## Dockerfile + supervisord.conf
To run multiple processes e.g. an Apache web server together with an SSH daemon inside the same container you can use ```supervisord```.

Create your ```supervisord.conf``` configuration file like:

    [supervisord]
    nodaemon=true
    
    [program:sshd]
    command=/usr/sbin/sshd -D
    
    [program:apache2]
    command=/bin/bash -c "source /etc/apache2/envvars && exec /usr/sbin/apache2 -DFOREGROUND"

Then create a ```Dockerfile``` like:
    
    FROM ubuntu:16.04
    RUN apt-get install -y openssh-server apache2 supervisor
    RUN mkdir -p /var/lock/apache2 /var/run/apache2 /var/run/sshd /var/log/supervisor
    COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
    CMD ["/usr/bin/supervisord"]

Then you can build your image:

    docker build -t supervisord-test .

Afterwards you can run it:

    $ docker run -p 22 -p 80 -t -i supervisord-test
    2016-07-26 13:15:21,101 CRIT Supervisor running as root (no user in config file)
    2016-07-26 13:15:21,101 WARN Included extra file     "/etc/supervisor/conf.d/supervisord.conf" during parsing
    2016-07-26 13:15:21,112 INFO supervisord started with pid 1
    2016-07-26 13:15:21,113 INFO spawned: 'sshd' with pid 6
    2016-07-26 13:15:21,115 INFO spawned: 'apache2' with pid 7
    ...


