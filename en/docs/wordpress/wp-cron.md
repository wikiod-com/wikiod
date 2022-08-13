---
title: "WP-Cron"
slug: "wp-cron"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## wp_schedule_event() example
    // register activation hook 
    register_activation_hook( __FILE__, 'example_activation' );

    // function for activation hook
    function example_activation() {
        // check if scheduled hook exists
        if ( !wp_next_scheduled( 'my_event' )) {
             // Schedules a hook
             // time() - the first time of an event to run ( UNIX timestamp format )
             // 'hourly' - recurrence ('hourly', 'twicedaily', 'daily' ) 
             // 'my_event' - the name of an action hook to execute. 
             wp_schedule_event( time(), 'hourly', 'my_event' );
        }
    }
 
    add_action( 'my_event', 'do_this_hourly' );

    // the code of your hourly event
    function do_this_hourly() {
       // put your code here
    }

    // register deactivation hook 
    register_deactivation_hook(__FILE__, 'example_deactivation');

    // function for deactivation hook
    function example_deactivation() {
        // clear scheduled hook
        wp_clear_scheduled_hook( 'my_event' );
    }

**Important:** the WordPress cron runs only when some page of your website is hit. So, for website with low trafic you need to setup the cron on your hosting to hit pages.

## custom recurrence interval in wp_schedule_event()
    
    // this function add custom interval (5 minutes) to the $schedules  
    function five_minutes_interval( $schedules ) {
          $schedules['five_minutes'] = array(
                'interval'  => 60 * 5,
                'display'   =>  '5 minutes';
        );
        return $schedules;
    }

    // add a custom interval filter
    add_filter( 'cron_schedules', 'five_minutes_interval' );

    // Schedules a hook
    wp_schedule_event( time(), 'five_minutes', 'my_event' );

