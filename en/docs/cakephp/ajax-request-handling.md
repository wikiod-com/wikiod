---
title: "Ajax request handling"
slug: "ajax-request-handling"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Basic CakePHP 2.x example
Controller:
In the Controller you have to add the RequestHandler component. This Enables CakePHP to automatically detect Ajax requests(see: http://book.cakephp.org/2.0/en/core-libraries/components/request-handling.html for more info):

    class YourController extends AppController {
        public $components = array('RequestHandler');
        //...

        public function ajaxCall() {
            if($this->request->is('ajax'){
                // some code that should be executed
                // ...
                // variables you want to return
                $this->set(compact('firstVariable', 'secondVariable'));
                $this->set('_serialize', array('firstVariable', secondVariable))
        }
    }

View Code (using jQuery):
        
    <script>
    $.ajax({
        type: 'POST',
        url: '/yourController/ajaxCall',
        success: function (result) {
            // result is a JSON array of the returned Variables
        },
        error: function (result){
            console.log(result);
        }
    });
    </script>

## Ajax Request in Cakephp 2.x
One more  way to use ajax in cakephp. Cakephp provide itself for ajax request. Please see example.

     $data = $this->Js->get('#id')->serializeForm(array('isForm' => true, 'inline' => true));
    //on click send request to controller and displays response data in view
    $this->Js->get('#button-id')->event(
            'click', $this->Js->request(
                    array('controller' => 'Users', 'action' => 'add'), array(
                'update' => '#time', // field you wish to update
                'data' => $data, // Form Data in serialize form
                'async' => true,    
                'dataExpression'=>true,
                'method' => 'POST' // method get or post
                    )
            )
    );


