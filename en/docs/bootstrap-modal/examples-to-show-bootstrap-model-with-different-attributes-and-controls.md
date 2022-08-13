---
title: "Examples to Show bootstrap model with different attributes and Controls"
slug: "examples-to-show-bootstrap-model-with-different-attributes-and-controls"
draft: false
images: []
weight: 9904
type: docs
toc: true
---

Given Below are the examples to show how bootstrap model can be shown and how to manage title main content and footer.

This topic is created to demonstrate different ways of showing and easily managing bootstrap model.

## Bootstrap Dialog with Title and Message Only
Include below CSS to your code

    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/css/bootstrap-dialog.min.css"/>

HTML Code for your Model to show easirly

    Click Button next to see bootstrap popup with simple message and Title.
        <button class="btn btn-primary" type="button" onclick="showModalNow()">Click Me</button>

Include script below in your code to get bootstrap-model working easily

    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/js/bootstrap-dialog.min.js" type="text/javascript" />
    <script>
        function showModalNow(){
            BootstrapDialog.show({
                title   : 'This is my custom Title',
                message : 'Hi Apple!'
            });
        }
    </script>




## Manipulating Dialog Title
Include below CSS to your code

    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/css/bootstrap-dialog.min.css"/>

HTML Code for your Model to show easily

    Click Button next to see bootstrap popup with simple message and Title and Buttons that manipulates the title.
        <button class="btn btn-primary" type="button" onclick="showModalNow()">Click Me</button>

Include script below in your code to get bootstrap-model working easily

    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.34.7/js/bootstrap-dialog.min.js" type="text/javascript" />
    <script>
        function showModalNow(){
            BootstrapDialog.show({
                title: 'Default Title',
                message: 'First time showing Title.',
                buttons: [{
                    label: 'change to Title 1',
                    action: function(dialog) {
                        dialog.setTitle('Title-1');
                    }
                }, {
                    label: 'change to Title 2',
                    action: function(dialog) {
                        dialog.setTitle('Title 2');
                    }
                }]
            });
        }
    </script>




## Show a div from same page as message
This a sample code which will show you how you can create dynamic code which will generate a bootstrap model at run time. This also includes deciding modal title body content and buttons in footer.

`HTML` code

    <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.35.3/css/bootstrap-dialog.min.css" rel="stylesheet">
    
    <script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap3-dialog/1.35.3/js/bootstrap-dialog.min.js"></script>
    
    <!--  This is a container Div which contains all the div to show when bootstrap dialog opens -->
    <div style="display : none" id="hidden-div">
      <div id="simple-div">
        <h1>This is H1 Heading</h1>
        This is most simple coding
        <br>
        <button type="button" class="btn btn-primary" onclick="showDiv1()">Lavel-2 div-1</button>
        <button type="button" class="btn btn-primary" onclick="showDiv2()">Lavel-2 div-2</button>
      </div>
      <div class="container-fluid" id="complex-div">
        <h2>Panels with Contextual Classes</h2>
        <div class="panel-group">
          <div class="panel panel-default">
            <div class="panel-heading">Panel with panel-default class</div>
            <div class="panel-body">Panel Content</div>
          </div>
    
          <div class="panel panel-primary">
            <div class="panel-heading">Panel with panel-primary class</div>
            <div class="panel-body">Panel Content</div>
          </div>
    
          <div class="panel panel-success">
            <div class="panel-heading">Panel with panel-success class</div>
            <div class="panel-body">Panel Content</div>
          </div>
    
          <div class="panel panel-info">
            <div class="panel-heading">Panel with panel-info class</div>
            <div class="panel-body">Panel Content</div>
          </div>
    
          <div class="panel panel-warning">
            <div class="panel-heading">Panel with panel-warning class</div>
            <div class="panel-body">Panel Content</div>
          </div>
    
          <div class="panel panel-danger">
            <div class="panel-heading">Panel with panel-danger class</div>
            <div class="panel-body">Panel Content</div>
          </div>
        </div>
      </div>
    </div>
    
    &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;
    <button type="button" class="btn btn-primary" onclick="simpleSHow('Hello 1234')">Simple Div Show</button>
    &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;
    <button type="button" class="btn btn-primary" onclick="complexSHow('Complex 1234')">Complex Div Show</button>

`JQuery/Javascript`


    function simpleSHow(title)
    {
        var msg=$('#simple-div');
        
        BootstrapDialog.show({
            title : title,
            message: $('#simple-div'),
            onhide : function(){
                $('#hidden-div').append(msg);
            }
        });
    }
    
    function complexSHow(title)
    {
        var msg=$('#complex-div');
        
        BootstrapDialog.show({
            title : title,
            message: $('#complex-div'),
            onhide : function(){
                $('#hidden-div').append(msg);
            }
        });
    }



Code Explanation


`HTML` structure :

    <div>(id=hidden-div)(always hidden)
     |
     |---- <div>(id=simple-div)
     |  |
     |  |---- content within this div 
     |
     |---- <div>(id=complex-div)
     |  |
     |  |---- content within this div 

Here div with id "hidden-div" is a container div by which i mean that it contains different divs to be shown in different modals. And all child div within it can be shown to one or more model at a time.


`Javascript` code explanation :

Upon calling the function we copied the div we want to show to a variable to use at last. After that we call the basic syntax to dynamically create bootstrap modal in which as a messaage we directly give the reference of the div that we want to show.

This will grab that div from the hidden-div by that I mean div that we want to show will be removed from the container div that's why we have saved copy of it to msg variable. And when the popup gets closed we will add that div back to the container div as if we need to to show that modal more then once then we will have that div available to be show next time also. if we don't copy at the beginning and add it back then next time when we want to show that modal without refreshing the page the we will not have that div present there in container div.

Given above is a simple example to show multiple bootstrap div with in a single html page and also without even writing a single conventional line to show model.

