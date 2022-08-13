---
title: "Make API in Codeigniter"
slug: "make-api-in-codeigniter"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

CodeIgniter provide auto initialized  Output class which is very useful for creating API and differents type of documents output like .pdf, .csv, .image, etc...

**NOTE :-** Codeigniter default document type is HTML change it to application/json, API must be required type of json

## create the new controller with name API


    <?php
    
    defined('BASEPATH') OR exit('No direct script access allowed');
    
    class Api extends CI_Controller {
      //default value
      private $login_credential;
    
      function __construct() {
        parent::__construct();
        //for user authentication
        $this->load->library('session');
    
        //set page header type Json as default
        $this->output->set_content_type('application/json');
        //default credentials for user login
        $this->login_credential = array(
            'username'=>'admin',
            'password'=>'test@123',
            'email'=> 'domain@test.com'
          );
      }
    }
    ?>

## Retrieve some data from API add following function in API controller


    /*****************************
    @return all events
    ****************************/
    public function getallevents(){
      //get data from model
      $events = array(
        array('Event 1', '2015-04-03'),
        array('Event 2', '2015-04-03'),
        array('Event 3', '2015-06-16'),
        array('Event 4', '2015-06-29'),
        array('Event 5', '2015-07-04'),
        array('Event 6', '2015-12-25'),
        array('Event 7', '2016-01-01')
      );
      $this->output->set_output(json_encode(array('status'=>true,'events'=>$events)));
    }

**Postman view**
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/vOkg2.jpg

## log in user API for allow access of some private data for perticular user


    /*****************************
    login user
    @required : username and password via post method only
    @return user data if login successfull otherwise error message
    ****************************/
    public function login(){
      $username=$this->input->post('username');
      $password=$this->input->post('password');
      if($username && $password){
        //check username and password
        if($this->login_credential['username']==$username && $this->login_credential['password']==$password){
          //set user data to store in session
          $userdata = array(
            'username'  => $this->login_credential['username'],
            'email'     => $this->login_credential['email'],
            'logged_in' => true
          );
          //set session
          $this->session->set_userdata($userdata);
          //display log in successfull msg
          $this->output->set_output(json_encode(array('status'=>true,'msg'=>'log in successfully','data'=>$userdata))); 
        }else{
          //wrong username or password
          $this->output->set_output(json_encode(array('status'=>false,'msg'=>'invalid Username or password'))); 
        }
      }else{
        //when username and password not set
        $this->output->set_output(json_encode(array('status'=>false,'msg'=>'provide Username and password')));
      }
    }

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/Xr7ZM.jpg

## user log out api to destroy the session of loged in user
    /***************************
    log out user
    ***************************/
    public function logout(){
      //delete all session
      session_destroy();
      $this->output->set_output(json_encode(array('status'=>true,'msg'=>'log Out successfully')));
    }

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/bklmA.jpg

## create protected api
**This API not accessible for public user, authentication is required**

    /***************************
    this is protected api this is not accessible if you are not loged in
    ***************************/
    public function protectedapi(){
      if($this->session->userdata('logged_in')){
        //this section only accessible when user loged in
        $this->output->set_output(json_encode(array('status'=>true,'msg'=>'Access allowed')));
      }else{
        $this->output->set_output(json_encode(array('status'=>true,'msg'=>'Access denied')));
      }
    }

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/e42MM.jpg

