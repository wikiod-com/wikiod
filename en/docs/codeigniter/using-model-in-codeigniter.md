---
title: "Using Model in codeigniter"
slug: "using-model-in-codeigniter"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Calling Model function

**Syntax** 

    $this->load->model('model_name');
    $this->model_name->method_name();

**Practice** 

    $this->load->model('home_model');
    $this->home_model->get_data();

## Creating Model
Go to `application/model`

File name - **Home_model.php**   
Inside the file

    class Home_model extends CI_Model {
    
        public $variable;
    
        public function __construct()
        { 
            parent::__construct();
        }
    
        public function get_data()
        {
            $query = $this->db->get('table_name', 10);
            return $query->result_array();
        }
    }
And when you need to load this model:

    $this->load->model('home_model');
    $this->home_model->get_data();
Or If you would like your model assigned to a different object name you can specify it like this:

    $this->load->model('home_model', 'home');
    $this->home->get_data();

## Return Data to Controller
    public function get_username($uid)
    {
                $query = 
                $this->db->select('id')
                 ->select('name')
                 ->from('user_table')
                 ->where('id', $uid)
                 ->get();
            return $query->result_array();
    }

this will return the result with matched id and username to the controller.

## Loading Model
Syntax - `$this->load->model('model_name');`   
Practice -  `$this->load->model('home_model');`

If you would like your model assigned to a different object name you can specify it via the second parameter of the loading method:

Syntax -

      $this->load->model('model_name', 'foobar');
      $this->foobar->method(); 

  
Practice - 

     $this->load->model('home_model', 'home'); 
     $this->home->get_data(); 

   
   

## Passing data to model
**Syntax** 

    $array = array(
        '' => , 
        ); # can pass array 
    $singelData  = ''; # something just a filed value
    $this->load->model('model_name');
    $this->model_name->method_name($singelData, $array);

**Practice** 

    $array = array(
        'name' => 'codeigniter', 
        'version' => '3.0', 
        'isArray' => 'yes', 
        );
    $singelData  = 'using model'; # something just a filed value
    $this->load->model('home_model');
    $this->home_model->get_data($singelData, $array);

## Receiving data from controller


    public function method_name($single, $array)
    {
        echo $single;
        print_r($array);
    }

>***Beware with the order which pass from controller to model.*** 

