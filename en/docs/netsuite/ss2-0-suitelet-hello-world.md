---
title: "SS2.0 Suitelet Hello World"
slug: "ss20-suitelet-hello-world"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Basic Hello World Suitelet - Plain Text Response
    /**
    *@NApiVersion 2.x
    *@NScriptType Suitelet
    */
    
    define([],function() { // NetSuite's AMD pattern
        function onRequest_entry(context) { // Suitelet entry function receives a context obj
            context.response.write('Hello World'); // Write a response using the context obj
        }
        return {
            onRequest: onRequest_entry // Function assigned to entry point
        };
    });

