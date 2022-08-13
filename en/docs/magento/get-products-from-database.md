---
title: "Get Products from Database"
slug: "get-products-from-database"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Get product by sku
    $sku = 'sku-goes-here';
    $product = Mage::getModel('catalog/product')->loadByAttribute('sku', $sku);

## Get product by ID
    $id = 1;
    $product = Mage::getModel('catalog/product')->load($id);
    if($product->getId()){
        //product was found
    }

## Get product collection by attribute
    $collection = Mage::getModel('catalog/product')->getCollection();
    // Using operator
    $collection->addAttributeToFilter('status', array('eq' => 1)); 
    // Without operator (automatically uses 'equal' operator
    $collection->addAttributeToFilter('status', 1); 


## Product collection - LIKE query
    $collection = Mage::getModel('catalog/product')->getCollection();
    $collection->addAttributeToFilter('sku', array('like' => 'UX%'));

## Product collection  - with attributes
    //all attributes
    $collection = Mage::getModel('catalog/product')
        ->getCollection()
        ->addAttributeToSelect('*');
    //specific attributes
    $collection = Mage::getModel('catalog/product')
        ->getCollection()
        ->addAttributeToSelect('name');
    //certain attributes are special, such as price and images
    //for images, then you can use 'getMediaGalleryImages'
    $product->load('media_galley');

## Get product collection from a list of SKUs
`$skuList = array('SKU-1', 'SKU-2',...,'SKU-n);`


    $_productCollection = Mage::getModel('catalog/product')
    ->getCollection()
    ->addAttributeToFilter('sku', array('in' => $skuList));

**OR**

    $_productCollection = Mage::getResourceModel('catalog/product_collection')
    ->addAttributeToFilter('sku', array('in' => $skuList));

## Get data from product object
    // First load a product object

    $product->getSku();
    $product->getName();
    
    // Alternative method
    $product->getData('sku');
    $product->getData('name');

    


## Get data form product collection
    // First load a collection object

    foreach($collection as $product) {
        
        $product->getSku();
        $product->getName();
    
        // Alternative method
        $product->getData('sku');
        $product->getData('name'); 
    }    

## Check if the product was correctly loaded
    $productFound = ($product->getId() !== null)

## Get product ID by SKU
    $sku = 'some-sku';
    $productId = Mage::getModel('catalog/product')->getIdBySku($sku);
    if($productId){
       //sku exists
    }

## Set Limit in product collection
    $collection = Mage::getModel('catalog/product')
                ->getCollection()
                ->setPageSize(20)
                ->setCurPage(1);

