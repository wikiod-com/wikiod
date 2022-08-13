---
title: "Shell, CLI"
slug: "shell-cli"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

# Basics

 - You need to have a Linux command line or connect using SSH to your server in order to use shell scripts.
 - Go to your `MAGENTO_ROOT/shell`
 - Script can be run by typing i.e. 

```
php -f indexer.php help
```

# Core shell methods by files

 1. abstract.php

 1. indexer.php

 1. compiler.php

 1. log.php

# Custom php shell scripts
Sometimes we need to access Magento outside of a webbrowser to ommmit execution times or set different things that won't affect the frontend.

There are 2 ways to bootstrap Magento but only one is the Magento way. Read more above in examples section.


## Using shell without extending Mage_Shell_Abstract

## Bootstrapping Magento by calling:
```
require_once 'app/Mage.php';
Mage::app();
// Your code
```

This is the simplest way but not really the Magento way because we're not using class that extends `Mage_Shell_Abstract` - the class which when extended provides us with tools to parse command line arguments, calls `__applyPhpVariables()` in it's constructor (function parses .htaccess files and applies php settings to shell script).


## Using shell the Magento way - extend Mage_Shell_Abstract
## Magento way
File resides in `shell/custom.php`

```
<?php
require_once' abstract.php';

class Stackoverflow_Shell_Custom extends Mage_Shell_Abstract
{

protected $_argname = array();
 
    public function __construct() {
        parent::__construct();
 
        // Time limit to infinity
        set_time_limit(0);     
 
        // Get command line argument named "argname"
        // Accepts multiple values (comma separated)
        if($this->getArg('argname')) {
            $this->_argname = array_merge(
                $this->_argname,
                array_map(
                    'trim',
                    explode(',', $this->getArg('argname'))
                )
            );
        }
    }

 // Shell script point of entry
    public function run() {
 
    }
 
    // Usage help
    public function usageHelp()
    {
        return <<<USAGE
Usage:  php -f scriptname.php -- [options]
 
  --argname <argvalue>       Argument description
 
  help                   This help
 
USAGE;
    }
}
// Instantiate
$shell = new Stackoverflow_Shell_Custom();
 
// Initiate script
$shell->run();

}

```

## Performing Reindex from CLI
**View Status:**

    php indexer.php status

**Reindex All**

    php indexer.php reindexall

**Reindex Specific Index**

    php indexer.php --reindex CODE (see list below)

**List of Individual Codes**

| Index | Code |
| ------ | ------ |
| Product Attributes   | catalog_product_attribute   |
|Product Prices |catalog_product_price|
|Catalog URL Rewrites|catalog_url|
|Product Flat Data|catalog_product_flat|
|Category Flat Data|catalog_category_flat|
|Category Products|catalog_category_product|
|Catalog Search Index|catalogsearch_fulltext|
|Stock Status|cataloginventory_stock|      

