---
title: "Get category name from product page"
slug: "get-category-name-from-product-page"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Get the parent category
    $_cat = new Mage_Catalog_Block_Navigation();
    $curent_cat = $_cat->getCurrentCategory();
    $curent_cat_id = $curent_cat->getId();
    $parentId=Mage::getModel('catalog/category')->load($curent_cat_id)->getParentId();
    $parent = Mage::getModel('catalog/category')->load($parentId);
    $categorydaddy = $parent->getName();

## Get the current category
        $categoryName = Mage::registry('current_category')->getName();
        foreach ($categoryName as $_category):
           $categoryName = $_category->getName();
        endforeach;

