---
title: "How to set time zone in CodeIgniter"
slug: "how-to-set-time-zone-in-codeigniter"
draft: false
images: []
weight: 9740
type: docs
toc: true
---

## How to set the time zone in CodeIgniter
Placing `date_default_timezone_set('Asia/Kolkata');` on `config.php` above base URL also works.

PHP [List of Supported Time Zones][1]

***application/config.php***

    <?php
    
    defined('BASEPATH') OR exit('No direct script access allowed');
    
    date_default_timezone_set('Asia/Kolkata');

Another way I have found useful is if you wish to set a time zone for each user:

 - Create a `MY_Controller.php` file.

 - Create a column in your `user` table you can name it timezone or any thing you want to. So that way, when user selects his time zone, it can can be set to his timezone when login.

***application/core/MY_Controller.php***

    <?php
    
    class MY_Controller extends CI_Controller {
    
        public function __construct() {
            parent::__construct();
            $this->set_timezone();
        }
    
        public function set_timezone() {
            if ($this->session->userdata('user_id')) {
                $this->db->select('timezone');
                $this->db->from($this->db->dbprefix . 'user');
                $this->db->where('user_id', $this->session->userdata('user_id'));
                $query = $this->db->get();
                if ($query->num_rows() > 0) {
                    date_default_timezone_set($query->row()->timezone);
                } else {
                    return false;
                }
            }
        }
    }

Also, to get the list of time zones in PHP:

     $timezones = DateTimeZone::listIdentifiers(DateTimeZone::ALL);

     foreach ($timezones as $timezone) {
        echo $timezone;
        echo "<br />";
     }

 
  [1]: http://php.net/manual/en/timezones.php

## Another way to set timezone in codeigniter
To set the timezone in Codeigniter by extending date helper is an alternative way. For doing that need to follow the following two step activity.

 1. Extend date helper with the following function:


    if ( ! function_exists('now'))
    {
        /**
        * Get "now" time
        *
        * Returns time() based on the timezone parameter or on the
        * "time_reference" setting
        *
        * @param    string
        * @return    int
        */
        function now($timezone = NULL)
        {
            if (empty($timezone))
            {
                $timezone = config_item('time_reference');
            }
            if ($timezone === 'local' OR $timezone === date_default_timezone_get())
            {
                return time();
            }
            $datetime = new DateTime('now', new DateTimeZone($timezone));
            sscanf($datetime->format('j-n-Y G:i:s'), '%d-%d-%d %d:%d:%d', $day, $month, $year, $hour, $minute, $second);
            return mktime($hour, $minute, $second, $month, $day, $year);
        }
    }
 2. Now set the timezone as a value of `time_reference` of `config.php` like: `$config['time_reference'] = 'Asia/Dhaka';`

This is all set for using time zone.

>**FYI:** List of Timezone List is added in the first example. 

