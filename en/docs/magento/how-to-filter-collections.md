---
title: "How to Filter Collections"
slug: "how-to-filter-collections"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Parameters
|Parameter|Details|
|---|---|
|$addFieldToFilter(**$field**, $condition = null)|{ string } The field we are adding to the filter.|
|$addFieldToFilter($field, **$condition = null**)|{ mixed } The definition of the filter we will use.|
|addAttributeToFilter(**$attr**, $condition = null, $join = 'inner')|{ string } The field we are adding to the filter.|
|addAttributeToFilter($attr, **$condition = null**, $join = 'inner')|{ mixed } The definition of the filter we will use.|
|addAttributeToFilter($attr, $condition = null, **$join = 'inner'**)|{ ('inner','left') } The type of sql join to use when joining the EAV table.|

**Filter Comparison Arguments**

Magento also offers a flexible way of filtering using comparison operators as well. Here is a list of valid operators and their syntax:

All comparison arguments can be passed to the second parameter of either the `addFieldToFielter()` or `addAttributeToFilter()` methods.

    $collection_of_products->addAttributeToFilter('visible',array("eq"=>1));

|Comparison|Argument Array|Resulting SQL Snippet|
|---|---|---|
|Equals|array("eq"=>$var)|WHERE (\`my_field` = $var)|
|Not Equal|array("neq"=>$var)|WHERE (\`my_field` != $var)|
|Like|array("like"=>$var)|WHERE (\`my_field` LIKE $var)|
|Not Like|array("nlike"=>$var)|WHERE (\`my_field` NOT LIKE $var)|
|Is|array("is"=>$var)|WHERE (\`my_field` IS $var)|
|In|array("in"=>$var)|WHERE (\`my_field` IN($var))|
|Not In|array("nin"=>$var)|WHERE (\`my_field` NOT IN($var))|
|Null|array("null"=>true)|WHERE (\`my_field` IS NULL)|
|Not Null|array("notnull"=>true)|WHERE (\`my_field` IS NOT NULL)|
|Greater Than|array("gt"=>$var)|WHERE (\`my_field` > $var)|
|Less Than|array("lt"=>$var)|WHERE (\`my_field` < $var)|
|Greater Than or Equal|array("gteq"=>$var)|WHERE (\`my_field` >= $var)|
|Less Than or Equal|array("lteq"=>$var)|WHERE (\`my_field` <= $var)|
|Find in Set|array("finset"=>array($var))|WHERE (find_in_set($var,\`my_field`)
|From and To|array("from"=>$var1, "to"=>$var2)|WHERE (\`my_field\` >= $var1 AND \`my_field` <= $var2)|

## Filtering Collections
Magento has a powerful set of methods to filter collections. Since there are two types of Objects that can be contained in collections, we must first determine which type of data we are working with before we can filter it. Magento implements a EAV data model for entities such as products and categories. There is a different set of methods to use if we are filtering a collection of EAV Objects.

In Magento, Orders are not stored as EAV Objects. This makes the orders collection a good example for filtering a basic collection.

    $collection_of_orders = Mage::getModel('sales/order')->getCollection();
    $collection_of_orders->addFieldToFilter('status','processing');

If we look at the products collection, we can see that the products are stored in an EAV data model. We can easily filter by EAV attributes as well. 

    $collection_of_products = Mage::getModel('catalog/product')->getCollection();
    $collection_of_products->addAttributeToFilter('visible',1);



## Handling ANDs and ORs in Filters
When we query our data, we often need more than one filter to get the exact data set we are looking for. In SQL, we handle this with AND and OR clauses. We can achieve the same thing with collections.

To add an AND clause to your query, just simply add another method call. This will append the second filter to the original WHERE statement joining it with an AND.

    Mage::getModel('catalog/product')->getCollection()
            ->addFieldToFilter('sku',array('like'=>'a%'))
            ->addFieldToFilter('sku',array('like'=>'%b'));

The resulting WHERE clause will look like this:

    WHERE (e.sku like 'a%') AND (e.sku like '%b')

Now lets say we want all skus that start with 'a' OR end with 'b'. How do we add an OR clause? Thanks to Magento's collections, it is pretty straight forward. We add the filter as a second element in the filter array. 

    Mage::getModel('catalog/product')->getCollection()
            ->addFieldToFilter('sku', array(
                array('like'=>'a%'), 
                array('like'=>'%b')
            ));

Now, the resulting WHERE clause will look like this:

    WHERE (((e.sku like 'a%') or (e.sku like '%b'))) 




