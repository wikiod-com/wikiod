---
title: "Getting started with jQuery Validate plugin"
slug: "getting-started-with-jquery-validate-plugin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Include the plugin from a CDN or locally after including jQuery.  Then attach the `.validate()` method to the form in order to initialize the plugin.  Within this method you can define your rules, custom messages, and other options.

    <script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"></script>
    <script type="text/javascript" src="//cdnjs.cloudflare.com/ajax/libs/jquery-validate/1.14.0/jquery.validate.js"></script>
    <script>
        $(document).ready(function() {  // <-- ensure form's HTML is ready
        
            $("#myform").validate({  // <-- initialize plugin on the form.
                // your rules and other options,
                rules: {
                    first_name: {  // <-- this is the name attribute, NOT id
                        required: true
                    },
                    last_name: {
                        required: true
                    },
                    phone: {
                        required: true,
                        digits: true
                    }
                }
            });
        
        });
    </script>

Every input considered for validation must contain a unique `name` attribute; and this is how the plugin keeps track of the inputs.  The corresponding HTML markup:

    <form id="myform">
        <input type="text" name="first_name" /><br/>
        <input type="text" name="last_name" /><br/>
        <input type="text" name="phone" /><br/>
        <input type="submit" />
    </form>

