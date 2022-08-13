---
title: "Upgrading Magento"
slug: "upgrading-magento"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Upgrade Magento via Composer

Check your current magento version

    php bin/magento --version

Now Add the latest version to your composer.

    composer require magento/product-community-edition 2.1.6 --no-update
Run Composer Update This will ask for the username and password take from your credentials from your marketplace account.

    composer update

This will start process to start downloading and upgrading your magento

Finally Update you static content and remove var folder

    rm -rf var/di var/generation
    php bin/magento cache:flush
    php bin/magento setup:upgrade
    php bin/magento setup:di:compile
    php bin/magento indexer:reindex

Recheck your magento version.

