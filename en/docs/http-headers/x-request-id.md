---
title: "X-Request-ID"
slug: "x-request-id"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

The `X-Request-ID` header can be used to trace individual requests to a web service (such as a REST API) from the client to the server and its backends.

## Syntax
* X-Request-ID: < value >

A Client can send an HTTP header `X-Request-ID: some-value`. The server should use the provided value and provide it in any requests that it makes to backend services for the purpose of serving the initial the request. When sending the response, the server will return the same header back to the client. For the purpose of tracing, the server will include the value into its logs, to enable correlating requests and responses with the corresponding logs.

## nginx
Reverse proxies can detect if a client provides a X-Request-ID header, and pass it on to the backend server. If no such header is provided, it can provide a random value.    

````
map $http_x_request_id $reqid {                                                 
    default   $http_x_request_id;                                               
    ""        $request_id;                                                      
}                                                                               
````

The code above stores the Request ID in the variable `$reqid` from where it can be subsequently used in logs.

````
log_format trace '$remote_addr - $remote_user [$time_local] "$request" '        
                 '$status $body_bytes_sent "$http_referer" "$http_user_agent" ' 
                 '"$http_x_forwarded_for" $reqid';                              
````

It should also be passed on to the backend services

````
    location @proxy_to_app {
        proxy_set_header X-Request-ID $reqid;
        proxy_pass   http://backend;
        access_log /var/log/nginx/access_trace.log trace;
    }
````

## Heroku
Heroku will always pass on a `X-Request-ID` header send by the client, or generate its own.

See documentation at [HTTP Request IDs](https://devcenter.heroku.com/articles/http-request-id).    

## Django
When using Django as a web service framework, the package [`django-log-request-id`](https://pypi.python.org/pypi/django-log-request-id) can be used to parse and log request IDs.

Settings
````
MIDDLEWARE_CLASSES = (
    'log_request_id.middleware.RequestIDMiddleware',
    # ... other middleware goes here
)

LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'filters': {
        'request_id': {
            '()': 'log_request_id.filters.RequestIDFilter'
        }
    },
    'formatters': {
        'standard': {
            'format': '%(levelname)-8s [%(asctime)s] [%(request_id)s] %(name)s: %(message)s'
        },
    },
    'handlers': {
        'console': {
            'level': 'DEBUG',
            'class': 'logging.StreamHandler',
            'filters': ['request_id'],
            'formatter': 'standard',
        },
    },
    'loggers': {
        'myapp': {
            'handlers': ['console'],
            'level': 'DEBUG',
            'propagate': False,
        },
    }
}
````    

## Request ID (Request / Response)
The same `X-Request-ID` header can be sent by a client in a request, or by a server in a response.

    X-Request-ID: f9ed4675f1c53513c61a3b3b4e25b4c0

The value does not carry any inherent meaning, but is just a token to identify correlating requests and responses.

