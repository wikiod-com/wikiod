---
title: "Configurable products and their variants."
slug: "configurable-products-and-their-variants"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Get a parent product and their children.
Here i will show you how to fetch 
1. All parent(configuarble products)
2. A parent product and all of its children.

## Get a parent product.
We will start by making a simple class that gets all our parent(Configurable products)

    <?php
    
    namespace Test\Test\Controller\Test;
    
    use Magento\Framework\App\Action\Context;
    
    
    class Products extends \Magento\Framework\App\Action\Action
    {
    
            public function __construct(
            \Magento\Catalog\Model\ResourceModel\Product\CollectionFactory $_product_res_fac
            )
            {
                $this->_product_res_fac = $_product_res_fac;
            }

            public function getParentProducts()
            {
                return $this->_product_res_fac->create()->addAttributeToSelect('*')->addAttributeToFilter('type_id', ['eq' => 'configurable']);
            }

        }

As you see above our getParentProducts function will now return all configuarble products we currently have in our system.




## Get parent and child products.
Here we first fetch our parent product and the we will get all children products that this parent have.

    <?php
    
    namespace Test\Test\Controller\Test;
    
    use Magento\Framework\App\Action\Context;
    
    
    class Products extends \Magento\Framework\App\Action\Action
    {
    
            public function __construct(
            \Magento\Catalog\Model\Product $productModel
            )
            {
                $this->product= $productModel;
            }
    
            public function getParentProduct()
            {
                return $this->product->load("a product entity id goes here")
            }
    
            public function getChildProducts()
            {
                $_children = $this->getParentProduct()->getTypeInstance()->getUsedProducts($this->getParentProduct());
            }
    
        }

The function getChildProducts now returns a children collection so you would be able to run it through a foreach loop and get all product attributes that might be on it.

