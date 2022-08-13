---
title: "Laravel Docker"
slug: "laravel-docker"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

A challenge that every developer and development team faces is environment consistency. Laravel is one of the most popular PHP frameworks today. DDocker, on the other hand, is a virtualization method that eliminates *“works on my machine”* issues when cooperating on code with other developers. The two together create a fusion of **useful** and **powerful**. Although both of them do very different things, they can both be combined to create amazing products.

## Using Laradock
**Laradock** is a project that provides a ready to go contains tailored for Laravel use.

Download or clone Laradock in your project's root folder: 

    git clone https://github.com/Laradock/laradock.git

Change directory into Laradock and generate the `.env` file needed to run your configurations:

    cd laradock
    cp .env-example .env

You are now ready to run docker. The first time you run the container it will download all the need packages from the internet.
    
    docker-compose up -d nginx mysql redis beanstalkd

Now you can open your browser and view your project on `http://localhost`.

For the full Laradock documentation and configuration [click here][1].


  [1]: http://laradock.io

