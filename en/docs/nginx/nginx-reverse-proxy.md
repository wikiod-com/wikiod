---
title: "nginx reverse proxy"
slug: "nginx-reverse-proxy"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## simple reverse proxy
    # Define which servers to include in the load balancing scheme. 
    # It's best to use the servers' private IPs for better performance and security.

    upstream backend {
    
        ip_hash; 
        server 10.10.10.10 slow_start=30s max_fails=3 fail_timeout=15s;
        server 10.10.10.12 slow_start=30s max_fails=3 fail_timeout=15s;
    
        # Activates the cache for connections to upstream servers.
        keepalive 20;
    }
    
    # This server accepts all traffic to port 80 and passes it to the upstream. 
    # Notice that the upstream name and the proxy_pass need to match.
    
    server {
        listen 80; 
        server_name example.com;
    
        location / {
            proxy_pass http://backend/;
        }
    }

