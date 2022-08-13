---
title: "Como lidar com o código de resposta HTTP AJAX"
slug: "como-lidar-com-o-codigo-de-resposta-http-ajax"
description: "Além de .done, .fail e .always prometem retornos de chamada, que são acionados com base se a solicitação foi bem-sucedida ou não, há a opção de acionar uma função quando um código de status HTTP específico é retornado do servidor. Isso pode ser feito usando o parâmetro statusCode."
excerpt: "Além de .done, .fail e .always prometem retornos de chamada, que são acionados com base se a solicitação foi bem-sucedida ou não, há a opção de acionar uma função quando um código de status HTTP específico é retornado do servidor. Isso pode ser feito usando o parâmetro statusCode."
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

Além de <code>.done</code> <code>.fail</code> e <code>.always</code> promessa de retornos de chamada, que são acionados com base em se a solicitação foi bem-sucedida ou não, há o opção para acionar uma função quando um código de status HTTP específico é retornado do servidor. Isso pode ser feito usando o parâmetro <code>statusCode</code>.

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

Como a documentação oficial do jQuery afirma:

Se a solicitação for bem-sucedida, as funções de código de status terão os mesmos parâmetros que o retorno de chamada de sucesso; se resultar em um erro (incluindo redirecionamento 3xx), eles usam os mesmos parâmetros que o retorno de chamada <code>error</code>.
