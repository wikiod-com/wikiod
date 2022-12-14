---
title: "Templates HTTPGet and HTTPGetJSON"
slug: "templates-httpget-and-httpgetjson"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## HTTPGetJSON
HTTPGetJSON performs an HTTP request to the specified URL and returns a [jsonq.JsonQuery][1] object for use in the alert template. Example:

<!-- language: lang-none -->
    template example {
        {{ $ip := 8.8.8.8 }} 
        {{ $whoisURL := printf "http://whois.arin.net/rest/ip/%s" $ip }}
        {{ $whoisJQ := $.HTTPGetJSON $whoisURL }}
        IP {{$ip}} owner from ARIN is {{ $whoisJQ.String "net" "orgRef" "@name" }}
    }

In this case the $ip address is hard coded but in a real alert it would usually come from the alert tags using something like `{{ $ip := .Group.client_ip}}` where client_ip is a tag key whose value is an IP address.

The jsonq results are similar to the results generated by the [jq JSON processor](https://stedolan.github.io/jq/), so you can test in a BASH shell using:

    $ curl -H "Accept: application/json" http://whois.arin.net/rest/ip/8.8.8.8 | jq  ".net.orgRef"
    {
      "@handle": "GOGL",
      "@name": "Google Inc.",
      "$": "https://whois.arin.net/rest/org/GOGL"
    }



  [1]:https://godoc.org/github.com/jmoiron/jsonq

