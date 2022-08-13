---
title: "AJAX HTTP Yanıt Kodu Nasıl Kullanılır"
slug: "ajax-http-yanit-kodu-nasil-kullanilir"
description: "İsteğin başarılı olup olmamasına bağlı olarak tetiklenen .done, .fail ve .always söz geri çağrılarına ek olarak, sunucudan belirli bir HTTP Durum Kodu döndürüldüğünde bir işlevi tetikleme seçeneği vardır. Bu, statusCode parametresi kullanılarak yapılabilir."
excerpt: "İsteğin başarılı olup olmamasına bağlı olarak tetiklenen .done, .fail ve .always söz geri çağrılarına ek olarak, sunucudan belirli bir HTTP Durum Kodu döndürüldüğünde bir işlevi tetikleme seçeneği vardır. Bu, statusCode parametresi kullanılarak yapılabilir."
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

İsteğin başarılı olup olmamasına bağlı olarak tetiklenen <code>.done</code> <code>.fail</code> ve <code>.always</code> vaat geri aramalarına ek olarak, sunucudan belirli bir HTTP Durum Kodu döndürüldüğünde bir işlevi tetikleme seçeneği. Bu, <code>statusCode</code> parametresi kullanılarak yapılabilir.

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

Resmi jQuery belgelerinde belirtildiği gibi:

İstek başarılıysa, durum kodu işlevleri başarılı geri aramayla aynı parametreleri alır; bir hatayla sonuçlanırsa (3xx yönlendirmesi dahil), <code>hata</code> geri aramasıyla aynı parametreleri alırlar.
