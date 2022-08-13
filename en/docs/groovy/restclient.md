---
title: "RESTClient"
slug: "restclient"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Groovy's HTTP Client usage, examples and pitfalls.

## GET Request
    @Grab(group='org.codehaus.groovy.modules.http-builder', module='http-builder', version='0.7' )
    
    import groovyx.net.http.RESTClient
    
    try {
        def restClient = new RESTClient("http://weathers.co")
        def response = restClient.get(path: '/api.php', query: ['city': 'Prague'])
        println "Status     : ${response.status}"
        println "Body       : ${response.data.text}"
    } catch (Exception e) {
        println "Error      : ${e.statusCode}"
        println "Message    : ${e.response.data}"
    }



