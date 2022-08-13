---
title: "Meta Box"
slug: "meta-box"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Simple usage of a Meta Box in the wp-admin content editors

## Syntax
 - _x( 'Text', 'Description', 'textdomain') is used to add a description for the translation service instead of __( 'Text', 'textdomain') which is just the translation
- _ex( 'Text', 'Description', 'textdomain') is used to echo translated text with a description

The content inside the render meta box can be anything. Instead of the values being directly integrated, you can also use an `include` with a PHP template and use `set_query_var` method to pass data to it. The save would work the same way.

## A simple example with a regular input and a select input
    /**
    * Add meta box to post types.
    *
    * @since  1.0.0
    */
    function myplugin_add_meta_box() {
        // Set up the default post types/
        $types = array(
            'post',
        );

        // Optional filter for adding the meta box to more types. Uncomment to use.
        // $types = apply_filters( 'myplugin_meta_box_types', $types );

        // Add the meta box to the page
        add_meta_box(
            'myplugin-meta-box', // Meta Box Id. Can be anything.
            _x( 'Custom Meta', 'Custom Meta Box', 'myplugin' ), // The title of the meta box. Translation is optional. Can just be string.
            'myplugin_render_meta_box', // The render meta box function.
            $types, // Add the post types to which to add the meta box.
            'side', // Show on the side of edit.
            'high' // Show at top of edit.
        );
    }

    add_action( 'add_meta_boxes', 'myplugin_add_meta_box' );

    /**
    * Render the meta box.
    *
    * This shows examples of a basic input and a select inside a meta box. These can be anything.
    *
    * @since  1.0.0
    *
    * @param $post WP_Post The post being edited.
    */
    function myplugin_render_meta_box( $post ) {
        // Get the current post meta values for our custom meta box.
        $city  = get_post_meta( $post->ID, 'city', true ); // True is for returning a single value.
        $country = get_post_meta( $post->ID, 'country', true ); // True is for returning a single value.

        // Add the WP Nonce field for security.
        wp_nonce_field( plugin_basename( __FILE__ ), 'myplugin_meta_nonce' );
    ?>

    <p>
        <label for="city">
            <?php _ex( 'City', 'Custom Meta Box Template', 'myplugin' ); ?>
        </label>
        <input name="city" id="city" value="<?php echo $city; ?>" />
    </p>
    <p>
        <label for="country">
            <?php _ex( 'Country', 'Custom Meta Box Template', 'myplugin' ); ?>
        </label>
        <select name="country" id="country">
            <option value="United States" <?php selected( $country, 'United States' ); ?>><?php _ex( 'United States', 'Custom Meta Box Template', 'myplugin' ); ?></option>
            <option value="Mexico" <?php selected( $country, 'Mexico' ); ?>><?php _ex( 'Mexico', 'Custom Meta Box Template', 'myplugin' ); ?></option>
            <option value="Canada" <?php selected( $country, 'Canada' ); ?>><?php _ex( 'Canada', 'Custom Meta Box Template', 'myplugin' ); ?></option>
        </select>
    </p>

    <?php
    }

    /**
    * Save meta box data.
    *
    * @since  1.0.0
    *
    * @param $post_id int The Id of the Post being saved.
    */
    function myplugin_save_meta_data( $post_id ) {
        // Verify this is not an auto save.
        if ( defined( 'DOING_AUTOSAVE' ) && DOING_AUTOSAVE ) {
            return;
        }

        // Validate the meta box nonce for security.
        if ( ! isset( $_POST['myplugin_meta_nonce'] ) || ! wp_verify_nonce( $_POST['myplugin_meta_nonce'], plugin_basename( __FILE__ ) ) ) {
            return;
        }

        // Get the new values from the form.
        $city    = $_POST['city'];
        $country = $_POST['country'];

        // update_post_meta will add the value if it doesn't exist or update it if it does.
        update_post_meta( $post_id, 'city', $city );
        update_post_meta( $post_id, 'country', $country );

        /*
        * OPTIONAL ALTERNATIVE
        *
        * Instead of just using update_post_meta, you could also check the values and 
        * issue create/update/delete on the post meta value.
        */
        // $current_city_value = get_post_meta( $post_id, 'city', true ); // True is for returning a single value.
        //
        // // Add the post meta if it doesn't exist.
        // if ( $city && '' === $city ) {
        //     add_post_meta( $post_id, 'city', $city, true ); // True means the key is unique to the post. False is default and more than one can be added.
        // }
        // // Edit the post meta if it does exist and there is a new value. 
        // elseif ( $city && $city != $current_city_value ) {
        //     update_post_meta( $post_id, 'city', $city );
        // } 
        // // Delete the post meta if there is no new value. 
        // elseif ( '' === $city && $current_city_value ) {
        //     delete_post_meta( $post_id, 'city', $current_city_value ); // $current_city_value is optional and is used to differentiate between other values with the same key.
        // }
    }

    add_action( 'save_post', 'myplugin_save_meta_data' );

