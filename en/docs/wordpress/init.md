---
title: "init"
slug: "init"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
 1. add_action( 'init', callable $function )

`init` is an action hook that gets fired after WordPress has finished loading but before any HTTP headers are sent.

## Processing $_POST request data
add_action('init', 'process_post_data');

    function process_post_data() {
        if( isset( $_POST ) ) {
            // process $_POST data here
        }
    }

## Processing $_GET request data
    add_action('init', 'process_get_data');
    
    function process_get_data() {
        if( isset( $_GET ) ) {
            // process $_GET data here
        }
    }

## Registering a custom post type
    add_action( 'init', function() {
        register_post_type( 'event', array(
                'public' => true,
                'label'  => 'Events'
            );
        );
    });

Registers a new custom post type with a label `Events` and a slug `event`

