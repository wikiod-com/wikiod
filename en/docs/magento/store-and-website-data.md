---
title: "Store and Website Data"
slug: "store-and-website-data"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Get magento store and website related data

## Get current store data
    $store = Mage::app()->getStore();
    $storeId = Mage::app()->getStore()->getStoreId();
    $storeCode = Mage::app()->getStore()->getCode();
    $websiteId = Mage::app()->getStore()->getWebsiteId();
    $storeGroupId = Mage::app()->getStore()->getGroupId();
    $storeName = Mage::app()->getStore()->getName();
    $storeSortOrder = Mage::app()->getStore()->getSortOrder();
    $storeIsActive = Mage::app()->getStore()->getIsActive();

