---
title: "Admin Dashboard Widgets"
slug: "admin-dashboard-widgets"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

With an admin dashboard widget you are able to display any kind of information at the admin dashboard. You can make multiple widgets if you want.
You can add the code to the functions.php of your theme or to your plugin.

## Syntax
- add_action($tag, $function_to_add, $priority, $accepted_args);
- wp_add_dashboard_widget($widget_id, $widget_name, $callback, $control_callback, $callback_args);


## Parameters
| Parameter | Details |
| ------ | ------ |
| $tag  | (*string   required*) Name of the action where $function_to_add is hooked    |
| $function_to_add |  (*callable  required*)  Name of the function you want to call.   |
| $priority  | (*int  optional*)   Place of the function call in all functions of action (default = 10) |
| $accepted_args  | (*int   optional*)   Number of parameters the function accepts (default = 1)  |
| $widget_id  | (*string   required*)   Unique slug for your widget   |
| $widget_name  | (*string   required*)   Name of your widget (displayed in the head)  |
| $callback  |(*callable   required*)  Name of the function which displays die content of your widget  |
| $control_callback  | (*callable   optional*)   Name of the function which handles the widget options forms   |
| $callback_args  | (*array   optional*)   Parameters of the $control_callback function   |

## Simple widget (displays text)
This will add a simple widget which displays just a small message.

     add_action('wp_dashboard_setup', 'register_my_dashboard_widgets');
            
     function register_my_dashboard_widgets() {
         wp_add_dashboard_widget('myInfo_widget', 'Important Information', 'display_infoWidget');
     }
            
    function display_infoWidget() {
         echo '<p>At the first of february this site gets a new design.
         Therefore is wont be available this day. To see the current progress you can visit 
         <a href="http://www.justanexample.com" >this site</a></p>';
     }



