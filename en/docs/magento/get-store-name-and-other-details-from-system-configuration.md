---
title: "Get store name and other details from system configuration"
slug: "get-store-name-and-other-details-from-system-configuration"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Get the frontend name for the current store view
    Mage::app()->getStore()->getFrontendName();

## Get current store ID
    Mage::app()->getStore()->getStoreId();


## Get current store code
    Mage::app()->getStore()->getCode();

This returns store code, e.g. 'en' for a storefront that is setup for English and called 'en', and not the numerical id.

## Determine if store view is enabled
    Mage::app()->getStore()->getIsActive();

## Get website ID for current store
    Mage::app()->getStore()->getWebsiteId();

## Get the current store model
    Mage::app()->getStore();

Returns an instance of `Mage_Core_Model_Store`

## Get group name for store
    Mage::app()->getStore()->getGroup()->getName()

## Get all Magento stores
    Mage::app()->getStores();

Returns an array of `Mage_Core_Model_Store` models.

