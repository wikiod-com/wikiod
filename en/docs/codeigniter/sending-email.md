---
title: "Sending Email"
slug: "sending-email"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

In CodeIgniter 3 you have to include the parameter:

    $config['newline'] = "\r\n";

It just won't work without it. 

If you don't care about new lines and you're using CodeIgniter 2 then this config parameter is optional.

## Load The Email Library
First you need to load the email library.

Do this either in the controller file that will be sending the email:

    $this->load->library('email');

Or load it globally in the autoload.php file in the config folder:

    $autoload['libraries'] = array('email');

While you're there, you may want to load the email helper if you want to use some of CodeIgniter's built in shortcuts:

    $autoload['helper'] = array('email');

The email helper can be loaded in the Controller file in a similar way to the email library:

    $this->load->helper('email');



## Set Your Email Config Parameters
Create a new file in the application/config folder named email.php

Set the parameters for sending email. These will load when you send your email.

    $config['newline'] = "\r\n"; //You must use double quotes on this one
    $config['protocol'] = 'smtp';
    $config['smtp_host'] = 'ssl://smtp.gmail.com'; //Change for your specific needs
    $config['smtp_port'] = 465; //Change for your specific needs
    $config['smtp_user'] = 'test@test.com'; //Change for your specific needs
    $config['smtp_pass'] = 'yourpassword'; //Change for your specific needs
    $config['charset'] = 'iso-8859-1';
    $config['mailtype'] = 'text'; //This can be set as 'html' too

## Create Your Email
    $this->email->from('accounts@yourwebsite.com', 'Tom Webmaster');
    $this->email->to('fred@fake.com', 'Freddie Fakeperson');
    $this->email->subject('Your Account Is Active');
    $this->email->message('Welcome to our new site!');

In the 'from' method, the first parameter is the email address your are sending from, the second parameter is the name you'd like the receiver to see. 

In the 'to' method, you define who the email is being sent to. 

The 'subject' method defines the subject of the email. 

The 'message' method defines what will be in the body of your email.

Any of these could be a data that was sent to your site by a user.  So you may have a variable in here that holds posted data. So they may look more like this:

    $this->email->to($email, $username);

## Send Your Email
    $sent = $this->email->send();


    //This is optional - but good when you're in a testing environment.
    if(isset($sent)){
        echo "It sent!";
    }else{
        echo "It did not send.";
    }

## Send An HTML Email
But you don't just want a plain text email. You want a pretty html email.

Set your config file as html:

    $config['mailtype'] = 'html';

If you want to pass data (like a username for example) to the html email, put them in an array:

    $data = array('name' => $name, 
                  'email' => $email,
                  'phone' => $phone,
                  'date' => $date);


Then when sending, point your 'message' to a view. Then pass your data array to it:

    $this->email->message($this->load->view('new_user',$data, true));


In your application/view folder create your view. 

In this case it's named 'new_user.php'. 

You can style this anyway you'd like. Here's a quick example:

    <html>
    <head>
        <style type='text/css'>
            body {background-color: #CCD9F9;
                 font-family: Verdana, Geneva, sans-serif}

            h3 {color:#4C628D}

            p {font-weight:bold}
        </style>
    </head>
    <body>

        <h3>Hi <?php echo $name;?>,</h3>
        <h3>Thanks for contacting us.</h3> 

        <p>You've taken your first step into a larger world.</p>   
        <p>We really appreciate your interest.</p>

    </body>
    </html>



## Contact Form
**Controller** (Pages.php)

    public function contact()
    {
        
        $this->load->library('email');
        $this->load->library('form_validation');

        //Set form validation
        $this->form_validation->set_rules('name', 'Name', 'trim|required|min_length[4]|max_length[16]');
        $this->form_validation->set_rules('email', 'Email', 'trim|required|valid_email|min_length[6]|max_length[60]');
        $this->form_validation->set_rules('message', 'Message', 'trim|required|min_length[12]|max_length[200]');

        //Run form validation
        if ($this->form_validation->run() === FALSE)
        {
            $this->load->view('contact');
        } else {

            //Get the form data
            $name = $this->input->post('name');
            $from_email = $this->input->post('email');
            $subject = $this->input->post('subject');
            $message = $this->input->post('message');

            //Web master email
            $to_email = 'admin@domain.com'; //Webmaster email, who receive mails

            //Mail settings
            $config['protocol'] = 'smtp';
            $config['smtp_host'] = 'ssl://smtp.gmail.com';
            $config['smtp_port'] = '465';
            $config['smtp_user'] = 'mail@domain.com'; // Your email address
            $config['smtp_pass'] = 'mailpassword'; // Your email account password
            $config['mailtype'] = 'html'; // or 'text'
            $config['charset'] = 'iso-8859-1';
            $config['wordwrap'] = TRUE; //No quotes required
            $config['newline'] = "\r\n"; //Double quotes required

            $this->email->initialize($config);                        

            //Send mail with data
            $this->email->from($from_email, $name);
            $this->email->to($to_email);
            $this->email->subject($subject);
            $this->email->message($message);
            
            if ($this->email->send())
            {
                $this->session->set_flashdata('msg','<div class="alert alert-success">Mail sent!</div>');

                redirect('contact');
            } else {
                $this->session->set_flashdata('msg','<div class="alert alert-danger">Problem in sending</div>');
                $this->load->view('contact');
            }

        }

**Views** (contact.php)

    <div class="container">
    <h2>Contact</h2>
    <div class="row">
        <div class="col-lg-6">
            <?php echo $this->session->flashdata('msg'); ?>
            <form action="<?php echo base_url('contact'); ?>" method="post">
            <div class="form-group">
                <input name="name" placeholder="Your name" type="text" value="<?php echo set_value('name'); ?>" class="form-control" />
                <?php echo form_error('name', '<span class="text-danger">','</span>'); ?>
            </div>
            <div class="form-group">
                <input name="email" placeholder="Your e-mail" type="text" value="<?php echo set_value('email'); ?>" class="form-control" />
                <?php echo form_error('email', '<span class="text-danger">','</span>'); ?>
            </div>
            <div class="form-group">
                <input name="subject" placeholder="Subject" type="text" value="<?php echo set_value('subject'); ?>" class="form-control" />
            </div>
            <div class="form-group">
                <textarea name="message" rows="4" class="form-control" placeholder="Your message"><?php echo set_value('message'); ?></textarea>
                <?php echo form_error('message', '<span class="text-danger">','</span>'); ?>
            </div>
            <button name="submit" type="submit" class="btn btn-primary" />Send</button>
            </form>
        </div>
    </div>
</div>

