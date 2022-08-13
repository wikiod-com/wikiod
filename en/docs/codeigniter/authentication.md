---
title: "Authentication"
slug: "authentication"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Loading Your Auth library for Every Controller
go to codeigniter/application/libraries/ create or replace your library files here.

go to codeigniter/application/core/ create a new php file named like MY_Controller.php

inside MY_Controller.php

    <?php 
    class MY_Controller extends CI_Controller{
        public function __construct(){
            parent::__construct();
            $this->load->library('AuthLib'); // AuthLib is your library name
        }
    }

  And then on every controller file you need to extends MY_Controller.

  Example of a controller; go to codeigniter/application/controllers and create a php file

    <?php 
        class Profile extends MY_Controller{
            public function __construct(){
                parent::__construct();
                if ($this->AuthLib->logged_in() === FALSE) { //if you wanna make this condition stament on every controller just  write it to inside construct function in MY_Controller.php 
                    redirect(base_url('/'));
                }
            }
        }



