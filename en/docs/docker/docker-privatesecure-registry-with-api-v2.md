---
title: "Docker privatesecure registry with API v2"
slug: "docker-privatesecure-registry-with-api-v2"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

A private and secure docker registry instead of a Docker Hub. Basic docker skills are required.

## Parameters
| Command | Explanation |
| ------ | ------ |
| sudo docker run -p 5000:5000 | Start a docker container and bind the port 5000 from container to the port 5000 of the physical machine.|
| --name registry | Container name (use to make “docker ps” readability better).|
| -v 'pwd'/certs:/certs | Bind CURRENT_DIR/certs of the physical machine on /certs of the container (like a “shared folder”).| 
| -e REGISTRY_HTTP_TLS_CERTIFICATE=/certs/server.crt | We specify that the registry should use /certs/server.crt file to start. (env variable)|
| -e REGISTRY_HTTP_TLS_KEY=/certs/server.key | Same for the RSA key (server.key).|
| -v /root/images:/var/lib/registry/ | If you want to save all your registry images you should do this on the physical machine. Here we save all images on /root/images on the physical machine. If you do this then you can stop and restart the registry without losing any images.|
| registry:2 | We specify that we would like to pull the registry image from docker hub (or locally), and we add « 2 » because we want install the version 2 of registry.|

[**How to install a docker-engine (called client on this tutorial)**][1]


[**How to generate SSL self-signed certificate**][2] 


  [1]: https://docs.docker.com/engine/installation/
  [2]: http://www.akadia.com/services/ssh_test_certificate.html

## Generating certificates
**Generate a RSA private key:** `openssl genrsa -des3 -out server.key 4096` 

Openssl should ask for a pass phrase at this step. Notice that we’ll use only certificate for communication and authentication, without pass phrase. Just use 123456 for example.

**Generate the Certificate Signing Request:** `openssl req -new -key server.key -out server.csr`

This step is important because you’ll be asked for some information about certificates. The most important information is “Common Name” that is the domain name, which be used for communication between private docker registry and all other machine. Example : mydomain.com

**Remove pass phrase from RSA private key:** `cp server.key server.key.org && openssl rsa -in server.key.org -out server.key`

Like I said we’ll focus on certificate without pass phrase. So be careful with all your key's files (.key,.csr,.crt) and keep them on a secure place.

**Generate the self-signed certificate:** `openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt`
 
You have now two essential files, *server.key* and *server.crt*, that are necessary for the private registry authentication.


## Run the registry with self-signed certificate
To run the private registry (securely) you have to generate a self-signed certificate, you can refer to previous example to generate it.

For my example I put *server.key* and *server.crt* into /root/certs

Before run docker command you should be placed (use `cd`) into the directory that contains *certs* folder. If you're not and you try to run the command you'll receive an error like 

> level=fatal msg="open /certs/server.crt: no such file or directory"

When you are (`cd /root` in my example), you can basically start the secure/private registry using  : <code>sudo docker run -p 5000:5000 --restart=always --name registry -v &#96;pwd&#96;/certs:/certs -e REGISTRY_HTTP_TLS_CERTIFICATE=/certs/server.crt -e REGISTRY_HTTP_TLS_KEY=/certs/server.key -v /root/Documents:/var/lib/registry/ registry:2</code><br/>
Explanations about the command is available on Parameters part.

## Pull or push from a docker client
When you get a working registry running you can pull or push images on it. For that you need the *server.crt* file into a special folder on your docker client. The certificate allows you to authenticate with the registry, and then encrypt communication. 

Copy *server.crt* from registry machine into /etc/docker/certs.d/mydomain.com:5000/ on your client machine.
And then rename it to *ca-certificates.crt* : `mv /etc/docker/certs.d/mydomain.com:5000/server.crt /etc/docker/certs.d/mydomain.com:5000/ca-certificates.crt`

At this point you can pull or push images from your private registry :<br/>
PULL : `docker pull mydomain.com:5000/nginx`
or <br/>PUSH : 

 1. Get an official image from hub.docker.com : `docker pull nginx`
 2. Tag this image before pushing into private registry : `docker tag IMAGE_ID mydomain.com:5000/nginx` (use `docker images` to get the IMAGE_ID)
 3. Push the image to the registry : `docker push mydomain.com:5000/nginx`


