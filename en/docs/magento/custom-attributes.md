---
title: "Custom Attributes"
slug: "custom-attributes"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Custom Attributes for sales, category, etc.

## Sales attribute
Custom attribute in sales related tables like: sales_flat_quote, sales_flat_order_item, sales_flat_order, etc table

In your installation file sql/some_setup/mysql-install-0.1.0.php:

    <?php
     $installer = $this; 
     $installer->startSetup();
     $installer->addAttribute('quote', 'custom_field', array('type' => 'varchar')); 
     $installer->addAttribute('order', 'custom_field', array('type' => 'varchar'));
     $installer->endSetup();
     ?>

Another way of doing it is:

    <?php
    $installer = $this;
    $installer->startSetup();
    $installer->run("ALTER TABLE sales_flat_order_item ADD COLUMN 'custom_field' DECIMAL(12,4) NULL;");
    $installer->endSetup();
    ?>

Make sure to clear cache after this.

