---
title: "Product Image Urls"
slug: "product-image-urls"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

Get product image urls for thumbnail, small image and base image. Get cached as well and non caches direct media urls.

## Cached Image urls
    Mage::helper('catalog/image')->init($item->getProduct(), 'thumbnail');
    Mage::helper('catalog/image')->init($item->getProduct(), 'small_image');
    Mage::helper('catalog/image')->init($item->getProduct(), 'image');

## Non cached Image Urls from Media
    Mage::getModel('catalog/product_media_config')->getMediaUrl($product->getThumbnail()); //Thumbnail
    Mage::getModel('catalog/product_media_config')->getMediaUrl($product->getSmallImage()); //Small Image
    Mage::getModel('catalog/product_media_config')->getMediaUrl($product->getImage()); //Base

