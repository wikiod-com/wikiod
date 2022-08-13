---
title: "AJAX"
slug: "ajax"
draft: false
images: []
weight: 9811
type: docs
toc: true
---

## AJAX with .ajax() and WordPress Nonce
**functions.php**

    //Localize the AJAX URL and Nonce
    add_action('wp_enqueue_scripts', 'example_localize_ajax');
    function example_localize_ajax(){
        wp_localize_script('jquery', 'ajax', array(
            'url' => admin_url('admin-ajax.php'),
            'nonce' => wp_create_nonce('example_ajax_nonce'),
        ));
    }

    //Example AJAX Function
    add_action('wp_ajax_example_function', 'example_function');
    add_action('wp_ajax_nopriv_example_function', 'example_function');
    function example_function(){
        if ( !wp_verify_nonce($_POST['nonce'], 'example_ajax_nonce') ){ 
            die('Permission Denied.'); 
        }
    
        $firstname = sanitize_text_field($_POST['data']['firstname']);
        $lastname = sanitize_text_field($_POST['data']['lastname']);
    
        //Do something with data here
        echo $firstname . ' ' . $lastname; //Echo for response
        wp_die(); // this is required to terminate immediately and return a proper response:- https://codex.wordpress.org/AJAX_in_Plugins
    }

**example.js**

    jQuery(document).on('click touch tap', '.example-selector', function(){
        jQuery.ajax({
            type: "POST",
            url: ajax.url,
            data: {
                nonce: ajax.nonce,
                action: 'example_function',
                data: {
                    firstname: 'John',
                    lastname: 'Doe'
                },
            },
            success: function(response){
                //Success
            },
            error: function(XMLHttpRequest, textStatus, errorThrown){
                //Error
            },
            timeout: 60000
        });
        
        return false;
    });

## AJAX request with a JSON response
**functions.php:**
```
// We add the action twice, once for logged in users and once for non logged in users.
add_action( 'wp_ajax_my_action', 'my_action_callback' );
add_action( 'wp_ajax_nopriv_my_action', 'my_action_callback' );

// Enqueue the script on the front end.
add_action( 'wp_enqueue_scripts', 'enqueue_my_action_script' );
// Enqueue the script on the back end (wp-admin)
add_action( 'admin_enqueue_scripts', 'enqueue_my_action_script' );

function my_action_callback() {
    $json = array();

    if ( isset( $_REQUEST['field2'] ) ) {
        $json['message'] = 'Success!';
        wp_send_json_success( $json );
    } else {
        $json['message'] = 'Field 2 was not set!';
        wp_send_json_error( $json );
    }
}

function enqueue_my_action_script() {
    wp_enqueue_script( 'my-action-script', 'path/to/my-action-script.js', array( 'jquery' ), null, true );
    wp_localize_script( 'my-action-script', 'my_action_data', array(
        'ajaxurl' => admin_url( 'admin-ajax.php' ),
    ) );
}
```

**my-action-script.js:**
```
(function( $ ) {
    'use strict';

    $( document ).on( 'ready', function() {
        var data = {
            action: 'my_action',
            field2: 'Hello World',
            field3: 3
        };

        $.getJSON( my_action_data.ajaxurl, data, function( json ) {
            if ( json.success ) {
                alert( 'yes!' );
            } else {
                alert( json.data.message );
            }
        } );
    } );

})( jQuery );

```

## wp_ajax - core functionality + _wpnonce check
**functions.php**:
<pre>
<code>
function rm_init_js() {
    wp_enqueue_script( 'custom-ajax-script', get_template_directory_uri() . '/js/ajax.js', array( 'jquery', 'wp-util' ), '1.0', true );
    // pass custom variables to JS
    wp_localize_script( 'custom-ajax-script', 'BEJS', array(
        'action' => 'custom_action',
        'nonce'  => wp_create_nonce( 'test-nonce' )
    ) );
}

add_action( 'wp_enqueue_scripts', 'rm_init_js' );

function rm_ajax_handler() {
    check_ajax_referer( 'test-nonce' );

    extract( $_POST );
    $data = compact( 'first_name', 'last_name', 'email' );

    foreach ( $data as $name => $value ) {
        switch ( $name ) {
            case 'first_name':
            case 'last_name':
                $data[ $name ] = ucfirst( sanitize_user( $value ) );
                break;
            case 'email':
                $data[ $name ] = sanitize_email( $value );
                break;
        }
    }

    $userID = email_exists( $data['email'] );

    if ( ! $userID ) {
        wp_send_json_error( sprintf( __( 'Something went wrong! %s try again!', 'textdomain' ), $data['first_name'] . ' ' . $data['last_name'] ) );
    }

    wp_update_user( array(
        'ID'           => $userID,
        'display_name' => $data['first_name'] . ' ' . $data['last_name'],
        'first_name'   => $data['first_name'],
        'last_name'    => $data['last_name'],
    ) );

    wp_send_json_success( sprintf( __( 'Welcome Back %s', 'textdomain' ), $data['first_name'] . ' ' . $data['last_name'] ) );
}

add_action( 'wp_ajax_custom_action', 'rm_ajax_handler' );
add_action( 'wp_ajax_nopriv_custom_action', 'rm_ajax_handler' );
</code>
</pre>

**ajax.js**
<pre>
<code>
;(function() {
    wp.ajax.post(BEJS.action, {
        first_name: 'john',
        last_name: '%65doe',
        email: 'john.doe@example.com',
        _ajax_nonce: BEJS.nonce
    }).done( function( response ) {
        alert(`Success: ${response}`);
    }).fail( function( response ) {
        alert(`Error: ${response}`);
    });
})();
</code>
</pre>

## OOP ajax submission using a simple class with nonce
You can copy and paste this whole plugin to try it. The class skeleton is used from 
[here][1].

 **class-oop-ajax.cpp**

    <?php
    
    /**
     * The plugin bootstrap file
     *
     * This file is read by WordPress to generate the plugin information in the plugin
     * Dashboard. This file defines a function that starts the plugin.
     *
     * @wordpress-plugin
     * Plugin Name:         Oop Ajax
     * Plugin URI:          http://
     * Description:         A simple example of using OOP principles to submit a form from the front end.
     * Version:             1.0.0
     * Author:              Digvijay Naruka
     * Author URI:          http://
     * License:             GPL-2.0+
     * License URI:         http://www.gnu.org/licenses/gpl-2.0.txt
     * Text Domain:         oop-ajax
     * Domain Path:         /languages
     */
    
    // If this file is called directly, abort.
    if ( ! defined( 'WPINC' ) ) {
        die;
    }
    
    class Oop_Ajax {
    
      // Put all your add_action, add_shortcode, add_filter functions in __construct()
      // For the callback name, use this: array($this,'<function name>')
      // <function name> is the name of the function within this class, so need not be globally unique
      // Some sample commonly used functions are included below
        public function __construct() {
       
            // Add Javascript and CSS for front-end display
            add_action('wp_enqueue_scripts', array($this,'enqueue'));
    
            // Add the shortcode for front-end form display
            add_action( 'init', array( $this, 'add_form_shortcode' ) );
            // Add ajax function that will receive the call back for logged in users
            add_action( 'wp_ajax_my_action', array( $this, 'my_action_callback') );
            // Add ajax function that will receive the call back for guest or not logged in users
            add_action( 'wp_ajax_nopriv_my_action', array( $this, 'my_action_callback') );
       
        }
    
        // This is an example of enqueuing a JavaScript file and a CSS file for use on the front end display
        public function enqueue() {
            // Actual enqueues, note the files are in the js and css folders
            // For scripts, make sure you are including the relevant dependencies (jquery in this case)
            wp_enqueue_script('my-ajax-script', plugins_url('js/oop-ajax.js', __FILE__), array('jquery'), '1.0', true);
    
            // Sometimes you want to have access to data on the front end in your Javascript file
            // Getting that requires this call. Always go ahead and include ajaxurl. Any other variables,
            // add to the array.
            // Then in the Javascript file, you can refer to it like this: my_php_variables.ajaxurl
            wp_localize_script( 'my-ajax-script', 'my_php_variables', array(
                'ajaxurl' => admin_url('admin-ajax.php'),
                'nonce' => wp_create_nonce('_wpnonce')
            ));
    
        }
    
        /**
         * Registers the shortcode for the form.
         */
        public function add_form_shortcode() {
    
            add_shortcode( "oop-ajax-add-form", array( $this, "add_form_front_end" ) );
    
        }
    
        /**
         * Processes shortcode oop-ajax-add-form
         *
         * @param   array    $atts        The attributes from the shortcode
         *
         * @return    mixed    $output        Output of the buffer
         */
        function add_form_front_end($atts, $content) {
    
            echo "<form id='my_form'>";
    
                echo "<label for='name'>Name: </label>";
                echo "<input id='name' type='text' name='name' ";
    
                echo "<br>" ;
    
                echo "<label id='email' for='email'>Email: </label>" ;
                echo "<input type='text' name='email'>";
    
                echo "<br>" ;
    
                echo "<input type='hidden' name='action' value='my_action' >" ;
                echo "<input id='submit_btn' type='submit' name='submit' value='submit'> ";
    
            echo "</form><br><br>";
            echo "<div id='response'>ajax responce will be here</div> ";
        }
        
         /**
         * Callback function for the my_action used in the form.
         *
         * Processses the data recieved from the form, and you can do whatever you want with it.
         *
         * @return    echo   response string about the completion of the ajax call.
         */
        function my_action_callback() {
            // echo wp_die('<pre>' . print_r($_REQUEST) . "<pre>");
    
            check_ajax_referer( '_wpnonce', 'security');
    
            if( ! empty( $_POST )){
    
                if ( isset( $_POST['name'] ) ) {
    
                    $name = sanitize_text_field( $_POST['name'] ) ;
                }
    
                if( isset( $_POST['email'] ) ) {
    
                    $email = sanitize_text_field( $_POST['email'] ) ;
                }
    
                ///////////////////////////////////////////
                // do stuff with values
                // example : validate and save in database
                //          process and output
                /////////////////////////////////////////// 

                $response = "Wow <strong style= 'color:red'>". $name . "!</style></strong> you rock, you just made ajax work with oop.";
                //this will send data back to the js function:
                echo $response;

            } else {

                echo "Uh oh! It seems I didn't eat today";
            }
    
            wp_die(); // required to terminate the call so, otherwise wordpress initiates the termination and outputs weird '0' at the end.
    
        }
    
    }
    //initialize our plugin
    global $plugin;
    
    // Create an instance of our class to kick off the whole thing
    $plugin = new Oop_Ajax();

**oop-ajax.js**

Put the js file inside the js directory i.e oop-ajax/js/oop-ajax.js
    

    (function($) {
        'use strict';
    
        $("#submit_btn").on('click', function() {
            // set the data
            var data = {
                 action: 'my_action',
                 security: my_php_variables.nonce,
                 name: $("#name").val(),
                 email: $("#email").val()
            }
    
            $.ajax({
                type: 'post',
                url: my_php_variables.ajaxurl,
                data: data,
                success: function(response) {
                    //output the response on success
                    $("#response").html(response);
    
                },
                error: function(err) {
                    console.log(err);
                }
            });
        
            return false;
        });
    })(jQuery);


  [1]: https://www.codementor.io/aaronoverton/wordpress-development-best-practices-oop-php-du107pcek

