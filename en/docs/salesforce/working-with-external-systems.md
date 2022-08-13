---
title: "Working with External Systems"
slug: "working-with-external-systems"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Making an outbound callout
This is an example on how to call a web service from salesforce. The code below is calling a REST based service hosted on data.gov to find farmers markets close to the zipcode.

Please remember in order to invoke a HTTP callout from your org, you need to tweak the remote settings for the org.


    string url= 'http://search.ams.usda.gov/farmersmarkets/v1/data.svc/zipSearch?zip=10017';
    Http h = new Http();
    HttpRequest req = new HttpRequest();
    HttpResponse res = new HttpResponse();
    req.setEndpoint(url);
    req.setMethod('GET');
    res = h.send(req);
    System.Debug('response body '+res.getBody());

