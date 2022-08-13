---
title: "Getting Magento URLs"
slug: "getting-magento-urls"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
 - $this->getSkinUrl('images/my-image.jpg');

## Parameters
| images path | details |
|-------------|---------|
| example: *'images/my-images.jpg'* | path for image |

Get formatted images url and avoid theme dependences.

## In current Interface/Theme
return http://www.example.com/skin/frontend/{interface}/{theme}/images/my-image.jpg

## Get Skin Url
    Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_SKIN);



## Get Base Url
    Mage::getBaseUrl();

## Secure Skin Url
    $this->getSkinUrl('images/imagename.gif', array('_secure'=>true));



## Get Media Url
    Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_MEDIA);



## Unsecure Skin Url
    $this->getSkinUrl('images/imagename.jpg');

## Get Store Url
    Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_WEB);



## Get Js Url
    Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_JS);



## Get Current Url
    Mage::helper('core/url')->getCurrentUrl();



