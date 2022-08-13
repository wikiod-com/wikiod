---
title: "Using Meteor with a Proxy Server"
slug: "using-meteor-with-a-proxy-server"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Using the `HTTP[S]_PROXY` env var
This page describes how to use the Meteor command-line tool (for example, when downloading packages, deploying your app, etc) behind a proxy server.

Like a lot of other command-line software, the Meteor tool reads the proxy configuration from the `HTTP_PROXY` and `HTTPS_PROXY` environment variables (the lower case variants work, too). Examples of running Meteor behind a proxy:

- on Linux or Mac OS X

```bash
export HTTP_PROXY=http://user:password@1.2.3.4:5678
export HTTPS_PROXY=http://user:password@1.2.3.4:5678
meteor update
```

- on Windows

```batch
SET HTTP_PROXY=http://user:password@1.2.3.4:5678
SET HTTPS_PROXY=http://user:password@1.2.3.4:5678
meteor update
```

## Setting Up a Proxy Tier
- [Deploy Meteor App to Ubuntu with Nginx Proxy](https://www.digitalocean.com/community/tutorials/how-to-deploy-a-meteor-js-application-on-ubuntu-14-04-with-nginx)

- [How to Create an SSL Certificate on Nginx for Ubuntu 14](https://www.digitalocean.com/community/tutorials/how-to-create-an-ssl-certificate-on-nginx-for-ubuntu-14-04)  

- [How to Deploy a Meteor JS App on Ubuntu with Nginx](https://www.digitalocean.com/community/tutorials/how-to-deploy-a-meteor-js-application-on-ubuntu-14-04-with-nginx)  

- [How to Install an SSL Certificate from a Commercial Certificate Authority](https://www.digitalocean.com/community/tutorials/how-to-install-an-ssl-certificate-from-a-commercial-certificate-authority)  

- [NameCheap SSL Certificates](https://www.namecheap.com/security/ssl-certificates/)



