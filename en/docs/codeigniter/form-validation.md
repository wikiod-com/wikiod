---
title: "Form Validation"
slug: "form-validation"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Validate Form Example
    //  initialize library
    $this->load->library('form_validation');  

    $this->form_validation->set_rules('username', 'Username', 'required|max_length[20]'); // Add validation rules for require and max
    $this->form_validation->set_rules('password', 'Password', 'required|matches[password]'); // Validation for the input match
    $this->form_validation->set_rules('passconf', 'Password Confirmation', 'required'); 
    $this->form_validation->set_rules('email', 'Email', 'required|valid_email|is_unique[userTable.emailColumn'); // add validation for the email and check the emailColumn in userTable for unique value
    $this->form_validation->set_message('is_unique', 'The %s is already taken, Please use another %s'); // add message for the is_unique

    if ($this->form_validation->run()  === FALSE)
    {
            // fail 
    }
    else
    {
            // success
    }
[Link][1]


  [1]: https://www.codeigniter.com/userguide3/libraries/form_validation.html

