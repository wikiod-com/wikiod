---
title: "Plugin development"
slug: "plugin-development"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
- add_action(string $tag, callable $function_to_add, int $priority = 10, int $accepted_args = 1)
- add_filter(string $tag, callable $function_to_add, int $priority = 10, int $accepted_args = 1)

## Parameters
| Parameter | Detail|
| ------ | ------ |
| $tag| (string) (Required) The name of the filter to hook the $function_to_add callback to.|
| $function_to_add | (callable) (Required) The callback to be run when the filter is applied. |
|$priority| (int) (Optional) Used to specify the order in which the functions associated with a particular action are executed. Lower numbers correspond with earlier execution, and functions with the same priority are executed in the order in which they were added to the action.Default value: 10 |
|$accepted_args | (int) (Optional) The number of arguments the function accepts.Default value: 1 |


The way Plugin hooks work is that at various times while WordPress is running, WordPress checks to see if any Plugins have registered functions to run at that time, and if so, the functions are run. These functions modify the default behavior of WordPress.


There are two kinds of hooks:

**Filters** give you the ability to change the value of a piece of data during the execution of WordPress. Callback functions for filters will be passed through a variable, modified, and then returned. They are meant to work in an isolated manner, and should never affect global variables or anything else outside of the function.

**Actions**, in contrast, allow you to add to or change how WordPress operates. Your callback function will run at a specific point in in the execution of WordPress, and can perform some kind of task, like echoing output to the user or inserting something into the database.

[Filter Reference][1]

[Action Reference][2]
  
[HandBook][3]

[Plugin API][4]

[Filters vs Actions][5]

  [1]: https://codex.wordpress.org/Plugin_API/Filter_Reference
  [2]: https://codex.wordpress.org/Plugin_API/Action_Reference
  [3]: https://developer.wordpress.org/plugins/hooks/
  [4]: https://codex.wordpress.org/Plugin_API
  [5]: http://ottopress.com/2011/actions-and-filters-are-not-the-same-thing/


## Filter
    add_filter('comment_text','before_comment');
    add_filter('comment_text','after_comment');
    function before_comment($comment_text){
                return 'input before comment'.$comment_text;
            }
    function after_comment($comment_text){
                return $comment_text.'input after comment';
            }

## Action
    add_action('wp_head','hook_javascript');

    function hook_javascript() {
        $output="<script> alert('Page is loading...'); </script>";
        echo $output;
    }


## Plugin development examples : Favorite Song Widget
        <?php
    function wpshout_register_widgets() {
        register_widget( 'Favorite_Song_Widget');
    }

    add_action( 'widgets_init', 'wpshout_register_widgets' );
    
    class Favorite_Song_Widget extends WP_Widget {

    function Favorite_Song_Widget() {
        // Instantiate the parent object
        parent::__construct(
                'favorite_song_widget', // Base ID
                __('Favorite Song', 'text_domain'), // Name
                array( 'description' => __( 'Widget for playable favorite song', 'text_domain' ), ) // Args
        );
    }

    function widget( $args, $instance ) {
        echo $args['before_widget']; 
        echo '<h3>Favorite Song Lists:</h3>';
        echo $instance['songinfo'];
        echo '<a href="' . $instance['link'] . '">Download it</a><br>';
        echo $instance['description'];
                echo $args['after_widget'];
    }

    function update($new_abc,$old_abc) {
        $instance = $old_abc;
        // Fields
        $instance['link'] = strip_tags($new_abc['link']);
        $instance['songinfo'] = strip_tags($new_abc['songinfo']);
                $instance['description'] = strip_tags($new_abc['description']);
        return $instance;
    }

    // Widget form creation
    function form($instance) {
         $link = '';
        $songinfo = '';
                $description = '';
        // Check values
        if( $instance) {
            $link = esc_attr($instance['link']);
            $songinfo = esc_textarea($instance['songinfo']);
                        $description = esc_textarea($instance['description']);
        } ?>
         

        <p>
            <label for="<?php echo $this->get_field_id('link'); ?>"><?php _e('Link', 'wp_widget_plugin'); ?></label>
            <input class="widefat" id="<?php echo $this->get_field_id('link'); ?>" name="<?php echo $this->get_field_name('link'); ?>" type="text" value="<?php echo $link; ?>" />
        </p>
         
        <p>
            <label for="<?php echo $this->get_field_id('songinfo'); ?>"><?php _e('Song Info:', 'wp_widget_plugin'); ?></label>
            <input class="widefat" id="<?php echo $this->get_field_id('songinfo'); ?>" name="<?php echo $this->get_field_name('songinfo'); ?>" type="text" value="<?php echo $songinfo; ?>" />
        </p>
                
                <p>
            <label for="<?php echo $this->get_field_id('description'); ?>"><?php _e('Description:', 'wp_widget_plugin'); ?></label>
            <textarea class="widefat" id="<?php echo $this->get_field_id('description'); ?>" name="<?php echo $this->get_field_name('description'); ?>" type="text" value="<?php echo $description; ?>"></textarea>
        </p>
                
                <p><a href="#" id="add-more-tabs"><?php _e('Add More Tabs', 'wp_widget_plugin'); ?></a></p>
        
    <?php }
}


