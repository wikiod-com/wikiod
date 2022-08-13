---
title: "Base url in Codeigniter"
slug: "base-url-in-codeigniter"
draft: false
images: []
weight: 9894
type: docs
toc: true
---

## Setting your base url in Codeigniter
You will need to set your base URL in `application/config/config.php`


If it is not set, then CodeIgniter will try to guess the protocol and path to
your installation, but due to the security concerns the hostname will be set
to `$_SERVER['SERVER_ADDR']` if available, or localhost otherwise.
The auto-detection mechanism exists only for convenience during
 development and MUST NOT be used in production!


    $config['base_url'] = '';

It should be filed like

    $config['base_url'] = 'http://localhost/projectname/';

    $config['base_url'] = 'http://www.example.com/';

Always good to use `/` at end of `base_url`

When you do not set your base URL you might run into some errors where you can not load your CSS, images, and other assets items. And also you might have trouble submitting forms as some users have come across.

**Update**

If you do not want to set your base URL another way is.

Create a new core file in `application/core/MY_Config.php`

And paste this code

    <?php
    
    class MY_Config extends CI_Config {
    
        public function __construct() {
    
            $this->config =& get_config();
    
            log_message('debug', "Config Class Initialized");
    
            // Set the base_url automatically if none was provided
    
            if ($this->config['base_url'] == '')
            {
                if (isset($_SERVER['HTTP_HOST']))
                {
                    $base_url = isset($_SERVER['HTTPS']) && strtolower($_SERVER['HTTPS']) !== 'off' ? 'https' : 'http';
                    $base_url .= '://'. $_SERVER['HTTP_HOST'];
                    $base_url .= str_replace(basename($_SERVER['SCRIPT_NAME']), '', $_SERVER['SCRIPT_NAME']);
                }
    
                else
                {
                    $base_url = 'http://localhost/';
                }
    
                $this->set_item('base_url', $base_url);
    
            }
        }
    }

## Something More About base_url
What happens if I don't set `base_url` ?
=====================================

You will not get any Impotency error to set this and proceed. You can continue without setting, but you should know about [HTTP header injection][1] 


----------


If I did't set it what will show up?
============================================

You will get  `http://[::1]/` instead of your actual URL.


----------


What does this mean `http://[::1]/` ??
=======================================

This is temporary URL which set by CI by Default. This will point the root of your document. 

`::1` - Server address (localhost) [Read More about this][2]


----------

How to set proper `base_url()`??
================================

Base URL should always point to root of your project folder. (outside application folder)

    $config['base_url'] = 'http://localhost/path/to/project'; # If localhost
    $config['base_url'] = 'http://stackoverflow.com/'; # If live
    $config['base_url'] = 'https://www.wikiod.com/docs/'; # If live & inside subdomain (assume documentation is subfolder/subdomain)
----------
How to use `base_url()`??
================================

Most common use is to find the right path to your js or css files. 

    <link rel="stylesheet" href="<?php echo base_url('styles/style.css');?>" />
    <script src="<?php echo base_url('vendor/jquery/jquery.min.js');?>"></script>

Adding the code above in your view will produce HTML as below:
    
    <link rel="stylesheet" href="http://localhost/path/to/project/styles/style.css" />
    <script src="http://localhost/path/to/project/vendor/jquery/jquery.min.js"></script>


----------

Links

 1. [URL Helper][3]

  [1]: https://en.wikipedia.org/wiki/HTTP_header_injection
  [2]: https://myousufali.wordpress.com/2012/06/26/nslookup-response-default-server-unknown-address-1/
  [3]: https://www.codeigniter.com/user_guide/helpers/url_helper.html

## Smart way to setting up the base_url
The following lines of code is more smart way to setting up the `base_url` in codeigniter:

    $config['base_url'] = ((isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] == "on") ? "https" : "http");
    $config['base_url'] .= "://".$_SERVER['HTTP_HOST'];
    $config['base_url'] .= str_replace(basename($_SERVER['SCRIPT_NAME']),"",$_SERVER['SCRIPT_NAME']);


----------


**Recommended is** 

    $config['base_url'] = 'https://stackoverflow.com/';

>Because everyone knows the hosting space. So if you set like this you ***can prevent Injection to your site/host***.

