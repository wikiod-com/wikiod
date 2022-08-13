---
title: "Wordpress blog integration with rails application using nginx"
slug: "wordpress-blog-integration-with-rails-application-using-nginx"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## nginx server block
Once you have setup and configured the php5-fpm and wordpress settings you can configure the /etc/nginx/conf/nginx.conf file as below.

You need to define the location blocks inside the server block and rewrite the url there as defined.

    server {
    listen 443 ssl;
            server_name abc.co.uk;
        root /home/ubuntu/www/abc/current/public;
            try_files $uri/index.html $uri @unicorn;
        ssl on;
        ssl_certificate /etc/nginx/ssl/abc.crt;
        ssl_certificate_key /etc/nginx/ssl/abc.key;
        location /blog/wp-admin/ { 
            root /var/www/html/;
            index index.php;
            try_files $uri $uri/ /index.php?$args;        
            location ~* \.(js|css|xml|txt|jpg)$ {
                 expires 14d;
             root /var/www/html/;
                 access_log off;
               }
        
        location ~ \.php$ {
                    try_files $uri $uri/ /index.php;
                    fastcgi_pass unix:/var/run/php5-fpm.sock;
                    fastcgi_param SCRIPT_FILENAME $request_filename;
                    fastcgi_index index.php;
                    include /etc/nginx/conf/fastcgi_params;
        }

    }
    location ^~ /blog {
        root /var/www/html/;
        index index.php;
        try_files $uri $uri/ /index.php?$args;        
        rewrite ^/blog/(.*)+$ /blog/index.php?$1;
        location ~* \.(js|css|xml|txt|jpg)$ {
             expires 14d;
         root /var/www/html/;
             access_log off;
           }
    
        location ~ \.php$ {
                    try_files $uri $uri/ /index.php;    
                    fastcgi_pass unix:/var/run/php5-fpm.sock;
                    fastcgi_param SCRIPT_FILENAME $request_filename;
                    fastcgi_index index.php;
                    include /etc/nginx/conf/fastcgi_params;
        }

      }



    }

