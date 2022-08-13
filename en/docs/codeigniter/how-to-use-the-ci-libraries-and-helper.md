---
title: "How to use the CI libraries and helper"
slug: "how-to-use-the-ci-libraries-and-helper"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Using libraries and helpers
The example is for illustration purpose of using libraries and helpers and not a valid code. Do not copy / paste it on your projects.

**HELPER** helpers/sendEmail_helper.php

    if ( ! function_exists('sendEmail'))
    {
        function sendEmail($email, $subject, $message, $lang, $cc = null, $file = null) {

            $CI =& get_instance();    
            
            $mail_config['protocol'] = 'smtp';
            $mail_config['smtp_host'] = 'host';
            $mail_config['smtp_user'] = 'user';
            $mail_config['smtp_pass'] = 'pass';
            $mail_config['smtp_port'] = '587';
            $mail_config['smtp_timeout'] = 5;
            $mail_config['charset'] = 'utf-8';
            $mail_config['mailtype'] = 'html';
            $mail_config['wrapchars'] = 76;
            $mail_config['wordwrap'] = TRUE;
                
            $CI->email->initialize($mail_config);
            $CI->email->set_newLine('\r\n');
            
            if ($lang == "en"){    
                $CI->email->from('support.en@domain.com', 'English Support');
            }else{
                $CI->email->from('support.fr@domain.com', 'Support en francais');
            }        
            $CI->email->to($email);
            if ($cc != null){
                $CI->email->cc($cc); 
            }    
            $CI->email->subject($subject);
            $CI->email->message($message);    
            if ($file != null){
                $CI->email->attach($file);    
            }    
            //$CI->email->print_debugger();
            return $CI->email->send();            
        }
    }

**LIBRARY** libraries/Alerter.php

    class Alerter {
    
        public function alert_user($user_email, $subject, $message, $lang) {
            //load helper
            $this->load->helper('sendEmail');
            //using helper
            sendEmail($user_email, $subject, $message, $lang);
        }
        
        public function alert_admin($admin_email, $subject, $message, $lang, $reason){
            //load helper
            $this->load->helper('sendEmail');
            .....
            //using helper
            sendEmail($admin_email, $subject, $message, $lang);
            .....
        }
    }

**CONTROLLER**

    class Alerts extends CI_Controller {
        function __construct() {
            parent::__construct();
        }  

        public function send_alert($userid) {
            
            //load library and model
            $this->load->library('Alerter');
            $this->load->model('alerter_model');
            
            //get user
            $user = $this->alerter_model->get_one_by_id($userid);
            
            //using library
            $this->Alerter->alert_user($user->email, $subject, $message, $lang);

        } 
    }

## Helper
Autoload your helper function. if you use many time in your project

    $autoload['helper'] = array('url', 'form');

Use form helper in view

    <?php echo form_open('Public/Login/loginAuth'); ?>
    
    <?php
           echo "<div class='row'>";
           echo "<label for='inputEmail' class='col-lg-2 control-label col-lg-offset-2 col-md-2 control-label col-md-offset-2 col-sm-2 control-label col-sm-offset-2'>Enter Email</label>";
           $email = array(
              "name"=>"email",
              "placeholder"=>"Email",
              "class"=>"form-control"
              );

         echo "<div class='col-lg-6 col-md-6 col-sm-6'>";
         echo form_error('email');
        echo form_input($email)."<br/>";
      echo "</div>";
    echo "</div>";
    
    echo "<div class='row'>";
        echo "<label for='inputPassword' class='col-lg-2 control-label col-lg-offset-2 col-md-2 control-label col-md-offset-2 col-sm-2 control-label col-sm-offset-2'>Enter Password</label>";
      $password = array(
        "name"=>"password",
        "placeholder"=>"Password",
          "class"=>"form-control"
        );

      echo "<div class='col-lg-6 col-md-6 col-sm-6'>";
        echo form_error('password');
        echo form_password($password)."<br/>";
      echo "</div>";
    echo "</div>";

    echo "<div class='row'>";
      
      $submit = array(
        "name"=>"submit",
        "value"=>"Submit",
        "class"=>"btn btn-primary col-lg-offset-9 col-md-offset-9 col-sm-offset-9 col-xs-offset-9"
        );
      echo form_submit($submit)."<br/>";
  
    echo "</div>";
?>
<?php echo form_close(); ?>

