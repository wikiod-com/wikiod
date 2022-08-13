---
title: "Let's start Hello World"
slug: "lets-start-hello-world"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## A very simple Hello World application
Starting from a fresh installation of Codeigniter 3, here is a simple way to start with an Hello World application, to break the ice with this solid PHP framework.

To do this you can start creating the view that we want to be shown for our Hello World app.

We are going to put it in your application folder, here:

**In `hello_world.php`**(`/application/views/`)

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="utf-8">
        <title>Hello World</title>
    </head>
    <body>

        <h1>Hello World!</h1>

    </body>
    </html>

It's just a simple HTML content.  

Now, in order to make this view shown, we need a **controller**. The controller is the one that will recall the view in order for its content to be displayed.

In order for it to work properly, the controller needs to go in the proper controllers folder.
 
Here is where we are going to place our Hello World controller: 

`/application/controllers/Hello_world.php`

(The controller's name is generally *snake_case* with the first letter uppercase)

    <?php
    defined('BASEPATH') OR exit('No direct script access allowed');
    
    class Hello_world extends CI_Controller {
 
        public function __construct()
        {
        parent::__construct();
        }

        public function index(){
            $this->load->view('hello_world');
        }

    }
        
The default function for a controller is the index function.
 
Now you will be able to see the content of your Hello World page accessing the following address:

    http://[your_domain_name]/index.php/hello_world

or, in case you applied the fix using .htaccess (go back to the installation page for the fix)

    http://[your_domain_name]/hello_world

(If you are working locally, most likely the address where you'll find your page is: `http://localhost/hello_world`) 

The URL is actually formed calling your controller class (in this case `Hello_world`, but using all lowercase in the URL). In this case it is enough, since we used the index function. If we would have used a different function name (let's say `greetings`), we should have used an URL like this:

    http://[your_domain_name]/hello_world/greetings

Which is structured as `/[controller_name]/[method_name]`.

Here you go! Your first Codeigniter application is working!

## Let's use the controller a little more
Now we'll try going for a little more complex example, using the capabilities of the controller to fill in the view.

Here is our view:
`/application/views/hello_world.php`

    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="utf-8">
        <title>Hello World</title>
    </head>
    <body>

    <h1><?php echo $greetings;?></h1>

    </body>
    </html>

Now we have a placeholder for our greetings to be displayed.

Here is how we change the controller in order for this to work:

    <?php
    defined('BASEPATH') OR exit('No direct script access allowed');
    
    class Hello_world extends CI_Controller {
        
        public function __construct() {
            parent::__construct();
        }
    
        public function greetings(){
            $data = array('greetings'=>'Hello World');
            $this->load->view('hello_world',$data);
        }
    }

The `$data` array is prepared with the information to be injected into the view, using the same label (`greetings`) that has been recalled inside the view.

The final result is the same as with the first example, but we are now using more of the potentiality of the framework!

## Let's choose our greetings: Hello World or Good Bye World or ...?
Let's say that we want to have an alternative greeting that is accessible through a different URL. We might create a new function or even a new controller for that, but a best practice is to optimize what we already have, to make it work at it's best!

To do this, we'll keep the same view as in the previous examples, but we'll introduce a parameter to our function, in order for it to be able to choose between two different greetings:

    <?php
    defined('BASEPATH') OR exit('No direct script access allowed');
    
    class Hello_world extends CI_Controller {
        
        public function __construct() {
            parent::__construct();
        }
    
        public function greetings($my_greetings){
            switch($my_greetings)
            {
                case 'goodbye':
                    $say = 'Good Bye World';
                break;
                case 'morning':
                    $say = 'Good Morning World';
                break;
                default:
                    $say = 'Hello World';
            }
            $data = array('greetings'=>$say);
            $this->load->view('hello_world',$data);
        }
    }

Now we have multiple greetings options! In order for them to be visualized, we are going to add the parameter at the URL, as follows:

    http://[your_domain_name]/hello_world/greetings/goodbye

This will show us the message: "Good Bye World".

The structure of the URL is as follows:

    http://[your_domain_name]/[controller_name]/[function_name]/[parameter_1]

In this case, in order to get back to our good old "Hello World", it's enough to call the former url, without parameters:

    http://localhost/hello_world/greetings

You can add multiple parameters to your function (for instance, if you need 3 of them):

    public function greetings($param1,$param2,$param3)

and they can be filled up using the url as follows:

    http://[your_domain_name]/[controller_name]/[function_name]/[param1]/[param2]/[param3]

e.g. `http://localhost/hello_world/greetings/goodbye/italian/red`

This way you can have parameters passed to you directly from the URL that will affect the content of what will be shown.

To know more about how to pass parameters through the URL, you might want look into the topic of routing!

