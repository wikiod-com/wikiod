---
title: "Cómo manejar el código de respuesta HTTP AJAX"
slug: "como-manejar-el-codigo-de-respuesta-http-ajax"
description: "Además de .done, .fail y .always prometen devoluciones de llamada, que se activan en función de si la solicitud fue exitosa o no, existe la opción de activar una función cuando el servidor devuelve un código de estado HTTP específico. Esto se puede hacer usando el parámetro statusCode."
excerpt: "Además de .done, .fail y .always prometen devoluciones de llamada, que se activan en función de si la solicitud fue exitosa o no, existe la opción de activar una función cuando el servidor devuelve un código de estado HTTP específico. Esto se puede hacer usando el parámetro statusCode."
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

Además de <code>.done</code> <code>.fail</code> y <code>.always</code> prometen devoluciones de llamadas, que se activan en función de si la solicitud fue exitosa o no, existe la opción para activar una función cuando se devuelve un código de estado HTTP específico del servidor. Esto se puede hacer usando el parámetro <code>statusCode</code>.

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

Como dice la documentación oficial de jQuery:

Si la solicitud es exitosa, las funciones del código de estado toman los mismos parámetros que la devolución de llamada exitosa; si da como resultado un error (incluida la redirección 3xx), toman los mismos parámetros que la devolución de llamada <code>error</code>.
