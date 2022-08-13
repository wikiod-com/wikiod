---
title: "Return a JSON Response with Slim"
slug: "return-a-json-response-with-slim"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Return a JSON Response with Slim
Slim can use withJson() that can return JSON responses with desired route and method

    $app = new \Slim\App();
    $app->get('/api/person', function ($request, $response, $args) {
    
    $payload=[];
    array_push($payload, array("name"=>"Bob"  ,"birth-year"=>1993));
    array_push($payload, array("name"=>"Alice","birth-year"=>1995));

     return $response->withJson($payload,200);

    });

    $app->run();

Try it with curl

    `curl -X GET http://127.0.0.1/api/person`

it can return an array in json

   ` [{"name":"Bob","birth-year":1993},{"name":"Alice","birth-year":1995}]`

## POST Request With Slim 3 (REST API)
<!-- language: php -->

    use \Psr\Http\Message\ServerRequestInterface as Request;
    use \Psr\Http\Message\ResponseInterface as Response;
    
    $app = new \Slim\App;
    
    $app->post('/post/data', function (Request $request, Response $response, $arg){
    
        $_input = $request->getParsedBody();
    
        $_data_1 = $_input['name'];
        $_data_2 = $_input['email'];
        $rsp = array();
    
        if(!empty($_data_1 && !empty($_data_2))){
    
            $rsp["error"] = false;
            $rsp['message'] = "hello my name is ".$_data_1." and my email is ".$_data_2;
        }else{
    
            $rsp["error"] = false;
            $rsp['message'] = "you have not posted any data" ;
        }
    
        return $response
            ->withStatus(201)
            ->withJson($rsp);
    });
    
    $app->run();

