---
title: "Magento 2 Commands for daily use"
slug: "magento-2-commands-for-daily-use"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

All the commands can be executed writting only part of them.

For example:

 - `php bin/magento cache:flush` can  be traslated to:
   - `php bin/magento c:f`
   - `php bin/magento ca:f`
   - `php bin/magento c:fl`
   - `php bin/magento cache:f`
   - `php bin/magento c:flush`
   - etc.

You can write any part, and if it is not ambiguos, it will automatically know which one you want.

## General List of Commands for Magento 2
    php bin/magento setup:upgrade                   => Setup Upgrade
    php bin/magento setup:di:compile                => Setup: Compile
    php bin/magento indexer:reindex                 => Reindex
    php bin/magento cache:flush                     => Clear Cache
    php bin/magento deploy:mode:set developer       => Enable Developer Mode Magento (developer/production)
    php bin/magento deploy:mode:show                => Show Current Mode Magento
    php bin/magento module:status                   => Module: Status
    php bin/magento module:disable MODULE_NAME      => Module: Disable
    php bin/magento module:enable MODULE_NAME       => Module: Enable
    php bin/magento module:uninstall MODULE_NAME    => Module: Uninstall
    php bin/magento cron:run                        => Cronjob: Run



## Flush  Cache
Flush all Magento Cache

    php bin/magento cache:clean
    php bin/magento cache:flush
    
Check cache status

    php bin/magento cache:status

## To see all available commands
    php bin/magento

## code compilation
    php bin/magento setup:di:compile

You might need to delete var/di (including the folder) in order to go through compilation.

    rm -rf var/di

## Enable Custom or 3rd Party Extensions
Enable and upgrade setup

    php bin/magento module:enable YKM_Custom
    php bin/magento setup:upgrade

Disable the Module

    php bin/magento module:disable YKM_Custom

Another One - *module uninstall script is executed and whole module gets deleted afterwards. Only modules installed through Composer can be uninstalled.*

    php bin/magento module:uninstall YKM_Custom

Display list of enabled and disabled modules
    
    php bin/magento module:status

## Update the database schema and data:
    php bin/magento setup:upgrade

