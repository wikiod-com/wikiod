---
title: "Comment gérer le code de réponse HTTP AJAX"
slug: "comment-gerer-le-code-de-reponse-http-ajax"
description: "En plus des rappels .done, .fail et .always promises, qui sont déclenchés selon que la demande a réussi ou non, il existe la possibilité de déclencher une fonction lorsqu'un code d'état HTTP spécifique est renvoyé par le serveur. Cela peut être fait en utilisant le paramètre statusCode."
excerpt: "En plus des rappels .done, .fail et .always promises, qui sont déclenchés selon que la demande a réussi ou non, il existe la possibilité de déclencher une fonction lorsqu'un code d'état HTTP spécifique est renvoyé par le serveur. Cela peut être fait en utilisant le paramètre statusCode."
date: 2021-11-04T09:19:42+01:00
lastmod: 2021-11-04T09:19:42+01:00
draft: false
weight: 50
images: []
categories: ["AJAX"]
tags: ["AJAX", "HTTP"]
pinned: false
homepage: false
---

En plus des rappels de promesse <code>.done</code> <code>.fail</code> et <code>.always</code>, qui sont déclenchés selon que la requête a réussi ou non, il y a le option pour déclencher une fonction lorsqu'un code d'état HTTP spécifique est renvoyé par le serveur. Cela peut être fait en utilisant le paramètre <code>statusCode</code>.

    $.ajax({
        type: {POST or GET or PUT etc.},
        url:  {server.url},
        data: {someData: true},
        statusCode: {
            404: function(responseObject, textStatus, jqXHR) {
                // No content found (404)
                // This code will be executed if the server returns a 404 response
            },
            503: function(responseObject, textStatus, errorThrown) {
                // Service Unavailable (503)
                // This code will be executed if the server returns a 503 response
            }           
        }
    })
    .done(function(data){
        alert(data);
    })
    .fail(function(jqXHR, textStatus){
        alert('Something went wrong: ' + textStatus);
    })
    .always(function(jqXHR, textStatus) {
        alert('Ajax request was finished')
    });

Comme l'indique la documentation officielle de jQuery :

Si la demande aboutit, les fonctions de code d'état prennent les mêmes paramètres que le rappel de succès ; si cela se traduit par une erreur (y compris la redirection 3xx), ils prennent les mêmes paramètres que le callback <code>error</code>.
