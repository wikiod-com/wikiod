---
title: "Response"
slug: "response"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Parameters
| Parameter| Details|
| ------ | ------ |
| `string` content | The response content.|
| `integer` status | The HTTP status code.|
| `array` headers | Array of response headers.|

## JsonResponse
Return JSON formated response: 

    use Symfony\Component\HttpFoundation\JsonResponse;
    
    public function someAction(){
        
        // Action's code

        $data = array(
            // Array data
        );

        return new JsonResponse($data); 
    }

## Simple usage
    public function someAction(){
            
        // Action's code 
            
        return new Response('<span>Hello world</span>'); 
    }
 
    
    

## Set status code
    public function someAction(){
        
        // Action's code 
    
        return new Response($error, 500); 
    }

Another example: 

    public function someAction(){
        
        // Action's code 
    
        $response = new Response(); 
        $response->setStatusCode(500)
        $response->setContent($content); 
        return $response;
    }

    



## Set header
See list of [http headers][1].

     public function someAction(){
        
        // Action's code
        $response = new Response(); 

        $response->headers->set('Content-Type', 'text/html');
    
        return $response; 
    }


  [1]: https://en.wikipedia.org/wiki/List_of_HTTP_header_fields

