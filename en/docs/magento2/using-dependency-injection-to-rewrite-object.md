---
title: "Using Dependency Injection To Rewrite Object"
slug: "using-dependency-injection-to-rewrite-object"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

https://gielberkers.com/magento-2-why-use-rewrites-when-you-can-use-plugins/

http://devdocs.magento.com/guides/v2.0/extension-dev-guide/plugins.html

## Some ways for modify a function in magento 2
    

# Rewrite Class

**File:** `Namespace/ModuleName/etc/di.xml`

    <?xml version="1.0"?>    
    <config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:ObjectManager/etc/config.xsd">
       <preference for="Magento\Catalog\Controller\Product\View" type="Namespace\ModuleName\Controller\Product\View" />
    </config>

**File:** `Namespace\ModuleName\Controller\Product\View.php`

    class View extends \Magento\Catalog\Block\Product\View
    {
        ///Code logic here
    }


# Plugin into object.

**File:** `Namespace/ModuleName/etc/di.xml`

        <?xml version="1.0"?>    
    <config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:ObjectManager/etc/config.xsd">
            <type name="Magento\Catalog\Model\Product">
                <plugin name="name_of_plugin" type="Namespace\ModuleName\Plugin\Catalog\Model\Product" sortOrder="1" disabled="false" />
            </type>
        </config>

**File:** `Namespace\ModuleName\Plugin\Catalog\Model\Product.php`

    namespace Namespace\ModuleName\Plugin\Catalog\Model;
     
    class Product
    {
        public function beforeSetName(
            \Magento\Catalog\Model\Product $product, string $name)
        {
            /// Code logic here   
            return $name;
        }
     
        public function afterGetName(
            \Magento\Catalog\Model\Product $product, string $name)
        {
             /// Code logic here    
            return $name;
        }
     
        public function aroundSave(
            \Magento\Catalog\Model\Product $product, \Closure $proceed)
        {
            $this->doSomethingBeforeSave();
            $result = $proceed();
            if ($result) {
                $this->doSomethingAfterSave();
            }
            return $result;
        }
    }

