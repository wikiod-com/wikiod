---
title: "Creating cronjob in codeigniter on linux hosting server"
slug: "creating-cronjob-in-codeigniter-on-linux-hosting-server"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Cronjob in Codeigniter
    <?php if ( ! defined('BASEPATH')) exit('No direct script access allowed');
    class Cron extends CI_Controller 
    {
        /**
         * This is default constructor of the class
         */
        public function __construct()
        {
            parent::__construct();
            $this->load->library('input');
            $this->load->model('cron_model');
        }
        
        /**
         * This function is used to update the age of users automatically
         * This function is called by cron job once in a day at midnight 00:00
         */
        public function updateAge()
        {            
            // is_cli_request() is provided by default input library of codeigniter
            if($this->input->is_cli_request())
            {            
                $this->cron_model->updateAge();
            }
            else
            {
                echo "You dont have access";
            }
        }
    }
    ?>


Call this from your cpanel/cron manager as follows (I added more ways to call it):

```0 0 0 0 0 php-cli /home/your_site_user/public_html/index.php cron updateAge```

OR

```0 0 0 0 0 wget http://your_site_url/cron/updateAge```

OR

```0 0 0 0 0 /usr/bin/php /home/your_site_user/public_html/index.php cron updateAge```


In my case:
wget thing is working on plesk and cpanel (wget creating files on server in your root directory).
php-cli works on plesk and cpanel both.

## Calling a CodeIgniter controller from cron
    // application/controllers/Company_controller.php
    <?php
    if(!defined('BASEPATH'))
        exit('No direct script access allowed');
    
    class Company_controller extends CI_Controller {
        public function __construct() {
            parent::__construct();
            $this->load->model('companies_model');    
        }
    
    // cron entry would be something like this:
    // 1  1  *  *  *  /usr/bin/php [full path to]/index.php company_controller cronCLI AcmeCorp >/dev/null 2>&1
        public function cronCLI($firmName) {
            if(php_sapi_name() == 'cli') {
                $this->companies_model->doSomeDB_Process($firmName);
            } else {
                echo 'CLI only';
            }
        }
    }



