---
title: "Cross Domain Request"
slug: "cross-domain-request"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Introduction
Sometimes we need cross domain request for our API's in laravel. We need to add appropriate headers to complete the cross domain request successfully. So we need to make sure that whatever headers we are adding should be accurate otherwise our API's become vulnerable.
In order to add headers we need to add middleware in laravel which will add the appropriate headers and forward the requests.
      

## CorsHeaders
    <?php
    
    namespace laravel\Http\Middleware;
    
    class CorsHeaders
    {
      /**
       * This must be executed _before_ the controller action since _after_ middleware isn't executed when exceptions are thrown and caught by global handlers.
       *
       * @param $request
       * @param \Closure $next
       * @param string [$checkWhitelist] true or false Is a string b/c of the way the arguments are supplied.
       * @return mixed
       */
      public function handle($request, \Closure $next, $checkWhitelist = 'true')
      {
        if ($checkWhitelist == 'true') {
          // Make sure the request origin domain matches one of ours before sending CORS response headers.
          $origin = $request->header('Origin');
          $matches = [];
          preg_match('/^(https?:\/\/)?([a-zA-Z\d]+\.)*(?<domain>[a-zA-Z\d-\.]+\.[a-z]{2,10})$/', $origin, $matches);
    
          if (isset($matches['domain']) && in_array($matches['domain'], ['yoursite.com']) {
            header('Access-Control-Allow-Origin: ' . $origin);
            header('Access-Control-Expose-Headers: Location');
            header('Access-Control-Allow-Credentials: true');
    
            // If a preflight request comes then add appropriate headers
            if ($request->method() === 'OPTIONS') {
              header('Access-Control-Allow-Methods: GET, POST, PUT, OPTIONS, DELETE, PATCH');
              header('Access-Control-Allow-Headers: ' . $request->header('Access-Control-Request-Headers'));
                // 20 days
              header('Access-Control-Max-Age: 1728000'); 
            }
          }
        } else {
          header('Access-Control-Allow-Origin: *');
        }
    
        return $next($request);
      }
    }

