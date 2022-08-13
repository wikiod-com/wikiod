---
title: "Getting started with docker-compose"
slug: "getting-started-with-docker-compose"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Ruby on Rails with docker-compose
If you want to use docker for rails app, and use database, you need to know that all the data in the docker container will be destroyed (unless you configure the container specifically for keeping data)
Sometimes, you need to create a docker container with an application and attach it to an old container with a database. 

As an example of rails application, I used a simple app. You can create it from command:

    rails new compose-app --database=postgresql
Of course, you need to install rails, ruby, etc. beforehand. 

Then, create Dockerfile in your project, and set this data to it:

    FROM ruby:2.3.1
    RUN apt-get update -qq && apt-get install -y build-essential libpq-dev nodejs
    RUN mkdir /compose-app
    WORKDIR /compose-app
    ADD Gemfile /compose-app/Gemfile
    ADD Gemfile.lock /compose-app/Gemfile.lock
    RUN bundle install
    ADD . /compose-app

Next step - create docker-compose.yml with the data:

    version: '2'
    services:
      db:
        image: postgres
      web:
        build: .
        command: bundle exec rails s -e development -p 80 -b '0.0.0.0'
        volumes:
          - .:/compose-app
        ports:
          - "80:80"
        depends_on:
          - db

You can replace 80 port (-p 80 ) with another.

Develop section of database.yml config must be changed to:

    development: &default
      adapter: postgresql
      encoding: unicode
      database: postgres
      pool: 5
      username: postgres
      password:
      host: db

Now you can build images from command:

    docker-compose build
(Run this in project directory)

And start all from:

    docker-compose up
If everything is done correctly, you will be able to see logs from rails in the console.

Close console. It will be working.

If you want to delete only the container with the rails application without the database, you need to run then in project directory:

    docker-compose stop web
    docker-compose build web
    docker-compose up -d --no-deps web

New container with rails app will be created and launched.

## Installation
If you are running Docker on OS X or Windows, docker-compose should be included in your [Docker for Windows][1] or Docker Toolbox installation.

On Linux you can get the latest binaries straight from the GitHub release page:
https://github.com/docker/compose/releases

You can install the specific release with the following commands:
```
curl -L https://github.com/docker/compose/releases/download/1.7.1/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose
```

For more info please refer to [documentation page][2]


  [1]: https://docs.docker.com/docker-for-windows/ "Docker for Windows"
  [2]: https://docs.docker.com/compose/overview/

## Create a simple application


## Install Docker Compose


## Run command in docker-compose service


## Docker Compose hello world
A very basic `docker-compose.yml` looks like this:

    version: '2'
    services:
      hello_world:
        image: ubuntu
        command: [/bin/echo, 'Hello world']

This file is making it so that there's a `hello_world` service, that's initialized from the `ubuntu:latest` image and that, when it's run, it just runs `echo 'Hello world'`

If you're on the `folder` directory (and it contains this `docker-compose.yml` file), you can do `docker-compose up` and you should see

    Creating folder_hello_world_1
    Attaching to folder_hello_world_1
    hello_world_1 | Hello world
    folder_hello_world_1 exited with code 0

This created the container from the ubuntu image, and ran the command that was specified on the `docker-compose.yml`

`Docker-Compose` uses the folder name as the project name to prefix containers and networks. To set another project name, you can either call `docker-compose --project-name NAME {up|down|...}` or you suppy a file called `.env` next to your `docker-compose.yml` and write `COMPOSE_PROJECT_NAME=name` in it. 
Better avoid long project names with hyphens (-) because docker compose bahaves strange with this kind of names.

Note: docker-compose allows you to run multiple docker containers on a single host. If you want to run multiple containers on more than one node, please refer to solution such as swarm / kubernetes. 

