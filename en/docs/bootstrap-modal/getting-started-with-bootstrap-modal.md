---
title: "Getting started with bootstrap-modal"
slug: "getting-started-with-bootstrap-modal"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Simple Message with in Bootstrap Modal
Code below will show you bootstrap model without writing hole long code by generating bootstrap model at run time. It will also make creating multiple bootstrap model i.e. one on other easy.
Follow below simple code to make simple bootstrap model.


Include below CSS to your code

    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/css/bootstrap-dialog.min.css"/>

HTML Code for your Model to show easirly

    Click Button next to see bootstrap popup with just simple message only.
        <button class="btn btn-primary" type="button" onclick="showModalNow()">Click Me</button>

Include script below in your code to get bootstrap-model working easily

<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/js/bootstrap-dialog.min.js" type="text/javascript" />
    <script>
        function showModalNow(){
            BootstrapDialog.show({
                message: 'Hi Apple!'
            });
        }
    </script>

## Installation or Setup
Bootstrap Model already comes with the bootstrap.min.js But to make it more simpler and more easy to execute We will need to follow steps below.

Installation : -

Just add below css and JS to your html page.

&lt;link href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/css/bootstrap-dialog.min.css" rel="stylsheet">

&lt;script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/js/bootstrap-dialog.min.js" type="text/javascript" />


Or 

You can download whole package from given below Link.

https://github.com/nakupanda/bootstrap3-dialog

