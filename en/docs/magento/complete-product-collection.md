---
title: "Complete Product Collection"
slug: "complete-product-collection"
draft: false
images: []
weight: 9999
type: docs
toc: true
---

Complete filters for product collection based on visibility,store,And OR, stock status, status, etc

## Filtering product collection
    $model = Mage::getModel('catalog/product')->getCollection()

Filter based on store:

    $mode->addStoreFilter($storeId)

Filter based on product type:

    $mode->addAttributeToFilter('type_id', 'configurable')
    $mode->addAttributeToFilter('type_id', 'simple')

Filter based on status:

    $model->addAttributeToFilter('status',Mage_Catalog_Model_Product_Status::STATUS_DISABLED)
    $model->addAttributeToFilter('status',Mage_Catalog_Model_Product_Status::STATUS_ENABLED)

Filter using null and notnull:

    $model->addAttributeToFilter('short_description', array('null' => true))
    $model->addAttributeToFilter('short_description', array('notnull' => true))
Filter using greater than and less than:

    $model->addAttributeToFilter('entity_id', array('gt' => 64230))
    $model->addAttributeToFilter('entity_id', array('lt' => 64230))

Filter using greater than and equal to:

    $model->addAttributeToFilter('entity_id', array('gteq' => 64230))
Filter using less than and equal to:

    $model->addAttributeToFilter('entity_id', array('lteq' => 64230))
Filter using in and not in:

    $model->addAttributeToFilter('entity_id', array('in' => array(1,4,64231)))
    $model->addAttributeToFilter('entity_id', array('nin' => array(1,4,64231)))
Filter products by a range of entity id:

    $model->addAttributeToFilter('entity_id', array(
                 'from' => 64229,
                 'to' => 64231
                 ))
Filter based on product visibility:

    $model->addAttributeToFilter('visibility', 4) //catalog,search
    $model->addAttributeToFilter('visibility', 3) //catalog
    $model->addAttributeToFilter('visibility', 2) //search
    $model->addAttributeToFilter('visibility', 1) //not visible individually
Filter using like and not like:

    $model->addAttributeToFilter('sku', array('nlike' => '5713%'))
    $model->addAttributeToFilter('sku', array('like' => '%shirt%'))
Filter using equal to and not equal to:

    $model->addAttributeToFilter('sku', array('neq' => 'shirt'))
    $model->addAttributeToFilter('sku', array('eq' => 'shirt'))
Filter in stock products:

    $model->joinField('is_in_stock',
                    'cataloginventory/stock_item',
                    'is_in_stock',
                    'product_id=entity_id',
                    'is_in_stock=1', //make this 0 for out of stock products
                    '{{table}}.stock_id=1',
                    'left')
Set order by:

    $model->setOrder('entity_id','desc')
Set Page Size:

    $model->setPageSize(100)

