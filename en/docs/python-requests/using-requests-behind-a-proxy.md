---
title: "Using requests behind a proxy"
slug: "using-requests-behind-a-proxy"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Setting proxy in Python code
If your code is running behind a proxy and you know the end point, you can set this information in your code. 

`requests` accepts a [`proxies`][1] parameter. This should be a dictionary that maps protocol to the proxy URL. 

    proxies = {
      'http': 'http://proxy.example.com:8080',
      'https': 'http://secureproxy.example.com:8090',
    }

Notice that in the dictionary we have defined the proxy URL for two separate protocols: HTTP and HTTPS. Each maps to an individual URL and port. This does not mean that the two can't be the same, though. This is also acceptable:

    proxies = {
      'http': 'http://secureproxy.example.com:8090',
      'https': 'http://secureproxy.example.com:8090',
    }

Once your dictionary is defined, you pass it as a parameter.

    requests.get('http://example.org', proxies=proxies)


  [1]: http://docs.python-requests.org/en/master/api/#requests.request

## Using proxy environment variables
`requests` uses specific environment variables automatically for proxy detection. 

- `HTTP_PROXY` will define the proxy URL to use for HTTP connections
- `HTTPS_PROXY` will define the proxy URL to use for HTTPS connections

Once these environment variables are set, the Python code does not need to pass anything to the `proxies` parameter. 

    requests.get('http://example.com')

