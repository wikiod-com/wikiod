---
title: "security"
slug: "security"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

In order to keep our images up to date for the security patches, we need to know from which base image we depend

## How to find from which image our image comes from
As an example, lets us look at a Wordpress container

The Dockerfile begins with 
FROM php:5.6-apache

so we go to the Dockerfile abovementioned
https://github.com/docker-library/php/blob/master/5.6/apache/Dockerfile

and we find
FROM debian:jessie
So this means that we a security patch appears for Debian jessie, we need to build again our image.

