---
title: "Getting started with ionic3"
slug: "getting-started-with-ionic3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
In this version change, Ionic has upgraded to Angular 4, a most recent version of TypeScript and some structural changes but starting an ionic project continues being similar to Ionic 2 so if you need more detailed information you could refer to that doc:


Ionic 2 Stack Overflow doc: https://www.wikiod.com/ionic2/getting-started-with-ionic2

Ionic official doc: http://ionicframework.com/getting-started

**Starting up Ionic 3 project**

Make sure you have the lastest version of Ionic:

    npm install -g ionic@latest

After that you can start your Ionic 3 project:

    ionic start

It will prompt you the name of the project and what type of Ionic Application you want to create, It also have some example projects.

If you don't want to initialize a git repo for the project, you have to use ionic start with the parameter --no-git

    ionic start --no-git

To test the application you can run the following command:
    
    ionic serve --lab
    
It will simulate the application in your default web browser.

