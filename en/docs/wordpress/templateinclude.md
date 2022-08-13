---
title: "template_include"
slug: "template_include"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Parameters
| Parameter | Explanation |
| ------ | ------ |
| `$template`   | Passes one parameter to the filter, `$template` is the current path to the appropriate file for the post type as found in the active child theme or parent theme (if no child theme in place or child theme has lower ranked templates. See wordpress template hierarchy for more details).   |

You must return `$template` even if not modifying. If this confuses you, look at examples where `apply_filter()` has been used in code

You should not set up variables here for use later, there are better hooks for this. 

A useful program flow for this filter is:

 1. Check `$template` includes our custom post type name --> template hierarchy!!
 2. if not, search our plugin for suitable files --> Its better to point to specific files rather than searching through folders for files. More efficient. But completely up to the developer. 
 3. return the template.

## Simple example
This filter is very useful. One of the common problems for developers is how to include templates in plugins they develop. 

The filter is applied immediately after wordpress locates the appropriate template in the active child/parent theme using the wp hierarchy. 

Be careful to define when you want to modify the template path. In the below example, the code checks to see if the current page is the single view of our custom post type `cpt`.

Simple enough example to get started with! 
 

    add_filter('template_include', 'custom_function');


    function custom_function($template){
        
        //change a single post template...
        
        if( is_singular('cpt')  ){
             $template= 'path/to/another/template_file';
        }
          
        
        return $template;

    }

## More Adv example
    add_filter('template_include', 'custom_function');


    function custom_function($template){
        
        /*
        *     This example is a little more advanced. 
        *    It will check to see if $template contains our post-type in the path.
        *    If it does, the theme contains a high level template e.g. single-cpt.php
        *    If not we look in the plugin parent folder for the file. e.g. single-cpt.php
        */
        
      
        //check to see if the post type is in the filename (high priority file)
        //return template if it is!
        
        global $post;
       
        if( strpos($template, 'single-'.$post->post_type.'php' ) !== false && strpos($template, 'archive-'.$post->post_type.'php' ) !== false ){
            return $template;
        }
        
        $plugin_path = 'var/etc/wp-content/plugins/plugin'; //include own logic here...
        
        if( is_singular($post->post_type) && file_exists($plugin_path.'/single-'.$post->post_type.'.php')   ){
             $template= $plugin_path.'/single-'.$post->post_type.'.php';
        } elseif ( is_archive($post->post_type) && file_exists($plugin_path.'/archive-'.$post->post_type.'.php') ) {
             $template= $plugin_path.'/archive-'.$post->post_type.'.php';
        } 
          
        return $template;
    }

