---
title: "Using Sessions"
slug: "using-sessions"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

The Codeigniter **Sessions** class uses browser cookies to save data that will persist across multiple page loads.

Reference: https://codeigniter.com/user_guide/libraries/sessions.html 

## Handling Session Data
A session is simply an array consisting of the following user information:

1. The user's unique Session ID (this is a statistically random string with very strong entropy, hashed with MD5 for portability, and regenerated (by default) every five minutes)
2. The user's IP Address
3. The user's User Agent data (the first 120 characters of the browser data string)
4. The "last activity" time stamp.

Source ([what-is-session-data][1])


----------


To retrieve session data
========================

 such as the SessionID:

    $this->session->userdata('session_id');

Note - for Codeigniter 3.x, you can use the above syntax, but the concept or magic getters has been introduced, where you can use `$this->session->session_id`.

Remember that the `userdata()` returns NULL if the session item doesn't exist.

To retrieve all session data

    $this->session->all_userdata()


----------


To Set Session Data
===================

the `set_userdata()` method allows you to set data into your session, the following example demonstrates an example array you wish to insert:

    $newdata = array(
            'username'  => 'johndoe',
            'email'     => 'johndoe@some-site.com',
            'logged_in' => TRUE
    );
    
    $this->session->set_userdata($newdata);

You can also set one data at a time, for example:

    $this->session->set_userdata('some_name', 'some_value');

or

    $some_name = 'some_value';
    $this->session->set_userdata($some_name);


----------


To Remove Session and Session Data
==================================

    $this->session->unset_userdata('some_name')

This method also accepts an array of item keys to unset:

For Codeiginter 3.x:

    $array_items = array('username', 'email');
    
    $this->session->unset_userdata($array_items);

For Codeiginter 2.x (this legacy syntax doesn't support 3.x):

    $array_items = array('key' => 'value');
    
    $this->session->unset_userdata($array_items);


  [1]: https://codeigniter.com/user_guide/libraries/sessions.html?#what-is-session-data

## Creating a Session
To Initialize a session, you can simply load it in your controller, this us usually placed inside the controller constructs, but it also can be autoloaded into the array found inside application/config/autoload.php:

    $this->load->library('session');


