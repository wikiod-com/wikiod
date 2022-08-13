---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Product Collection
    $productCollection = Mage::getModel('catalog/product')->getCollection();

**Selecting the specific Attribute** 
   
    $productCollection->addAttributeToSelect(array('name', 'product_url', 'small_image'));

**Selecting the All Attributes**  

    $productCollection->addAttributeToSelect('*');


**Add Filter on Collection**

    $productCollection->addFieldToFilter('is_active', 1);


**Set Order** 

    $productCollection->setOrder('id', 'ASC');

**Set limit**
   
    $productCollection->setPageSize(10);

**Set Current Page**

    $productCollection->setCurPage($page);


    

