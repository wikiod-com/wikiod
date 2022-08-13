---
title: "Current url"
slug: "current-url"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - `$this->helper('core/url')->getCurrentUrl();`

## Homepage
return : http://www.example.com/

## Product page
return : http://www.example.com/my-product.html

## Check if current url is secure
    $isSecure = Mage::app()->getStore()->isCurrentlySecure();
This will return true if the current url is secure.

