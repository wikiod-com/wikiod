---
title: "Helpers"
slug: "helpers"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Creating a helper
Helpers should extend from `Mage_Core_Helper_Abstract`:

    # File: app/code/local/Vendor/Package/Helper/Data.php
    class Vendor_Package_Helper_Data extends Mage_Core_Helper_Abstract
    {
        public function multiply($a, $b)
        {
            return $a * $b;
        }
    }

To be able to access is via `Mage::helper` you need to define a helper alias in a `config.xml` file to allow the Magento autoloader to find your class:

    <!-- File: app/code/local/Vendor/Package/etc/config.xml -->
    <global>
        <helpers>
            <alias_here>
                <class>Vendor_Package_Helper</class>
            </alias_here>
        </helpers>
    </global>

Assuming your module is correctly configured and you have cleared your cache, you should now be able to use your helper like so:

    $result = Mage::helper('alias_here')->multiply(2, 4); // int(8)

**Note:** if you're using a Data class, its helper name is implied if you don't specify one. For example, the following two examples are identical:

    Mage::helper('alias_here');
    Mage::helper('alias_here/data');

