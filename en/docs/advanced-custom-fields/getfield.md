---
title: "get_field()"
slug: "get_field"
draft: false
images: []
weight: 9739
type: docs
toc: true
---

## Syntax
- `get_field($field_name, $post_id, $format_value);`

## Parameters
| Parameter | Details |
| ------ | ------ |
| $field_name   | (string) The name of the field you are requesting. When setting up your field group, this is the "Field Name" setting.   |
| $post_id   | (integer) This is the ID of the post that you are requesting the field name from. This can also be 'options' or a taxonomy reference. This can be left blank when inside of the WordPress loop, if requesting the current post/page ID.  |
| $format_value   | Determine if you want to format the value loaded from the database. Defaults to true. Not required.   |

**get_field() vs. the_field()**

the_field() automatically displays the field value on your page, where get_field() does not. Use get_field() when assigning field values to variables, or when manipulating the returned content in your code.

## Sanitize get_field() output
It's a good idea to sanitize get_field() output, especially when using Advanced Custom Fields fields front end (with acf_form()). Otherwise your site is likely vulnerable to cross-site scripting attacks (XSS).

The following function lets you use 

    echo get_field_escaped('my_custom_field', $post_id, true);

instead of

    echo get_field('my_custom_field', $post_id, true);

The function uses esc_html as default, but let's you change this as a fourth parameter

    echo get_field_escaped('url', $post_id, true, 'esc_url');

Add the following to `functions.php` to enable the function:

    /**
     * Helper function to get escaped field from ACF
     * and also normalize values.
     *
     * @param $field_key
     * @param bool $post_id
     * @param bool $format_value
     * @param string $escape_method esc_html / esc_attr or NULL for none
     * @return array|bool|string
     */
    function get_field_escaped($field_key, $post_id = false, $format_value = true, $escape_method = 'esc_html')
    {
        $field = get_field($field_key, $post_id, $format_value);
     
        /* Check for null and falsy values and always return space */
        if($field === NULL || $field === FALSE)
            $field = '';
     
        /* Handle arrays */
        if(is_array($field))
        {
            $field_escaped = array();
            foreach($field as $key => $value)
            {
                $field_escaped[$key] = ($escape_method === NULL) ? $value : $escape_method($value);
            }
            return $field_escaped;
        }
        else
            return ($escape_method === NULL) ? $field : $escape_method($field);
    }

Source: https://snippets.khromov.se/sanitizing-and-securing-advanced-custom-fields-output/

More about the different sanitization options in the WordPress Codex: https://codex.wordpress.org/Data_Validation#Output_Sanitization

## Simple Example
    <?php echo get_field('my_field_name'); ?>
This will echo the value of the field "my_field_name" from the current post.

## Get Field Value from a Different Post
    <?php echo get_field('my_field_name', 123); ?>
This will echo the value of "my_field_name" from the post with 123 as its ID.

## Get Field Value from an Options Page
    <?php echo get_field('my_field_name', 'option'); ?>
This will echo the value of "my_field_name" from the options page created via ACF.

## Check if Field Exists
    <?php if( get_field('my_field_name') ){ ?>
        <?php echo get_field('my_field_name'); ?>
    <?php }; ?> 

This will only show the field if content exists depending on content type (i.e., an image is uploaded to the field, text is entered, it is selected, etc.).

## Get Field With Radio Buttons
Here's an example on how to use ACF to output differently based on options (color selections in this case). While you can use `<?php echo get_field('color_options'); ?>` to output the value directly, you can also change the markup depending on the selection.

    <?php $option = get_field('color_options'); ?>
        
        <?php if( $option == 'red' ){ ?>
        
        <?php } else if( $option == 'blue' ){ ?>
            
        <?php } else if( $option == 'green' ){ ?>
            
        <?php } else if( $option == 'yellow' ){ ?>
    
    <?php } ?>

