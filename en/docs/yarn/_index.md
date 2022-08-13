---
title : yarn Tutorial
slug : yarn-tutorial
weight : 9960
draft : false
images : []
type : docs
---

## **Why Yarn, when we have NPM?**

This is the burning question now. NPM works great for thousands of developers but it won’t work that great for companies like Facebook , Google. If you’ve deleted your node_modules folder for any reason and run `npm install` in the project console, npm will re download each and every package along with their dependencies which is too much time killing. Yarn is great in this purpose. It caches every package it downloads. If you have ever downloaded the package before, you can install it in offline mode too.It also parallelizes operations to maximize resource utilization so install time are faster than ever, like the rocket trying to escape the earth’s gravity!
Yarn is super secured. It uses checksums to verify the integrity of every installed package before its code is executed.
Yarn is reliable . According to their voice, “ Yarn is able to guarantee that an install that worked on one system will work exactly the same way on any other system.”

