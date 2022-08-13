---
title: "Understanding product types"
slug: "understanding-product-types"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

There are six different product types built-in to Magento.

 - Simple

A single stock unit

 - Configurable

First of the composite products. Allow customers to configure their product and add a single simple product to basket.

 - Grouped

The second composite product, a grouped product relates simple products and provides customers with the ability to choose quantities of each item.

 - Bundle

The third composite product type , a bundle relates simple products together to purchase as a single item.

 - Virtual

No physical item required for delivery, e.g. services

 - Downloadable

A digital rather than physical product.
Most product types are implemented as part of the Mage_Catalog module, apart from Mage_Bundle and Mage_Downloadable.

Grouped, Bundle and Configurable products implement a parent-child relationship where a number of other (by default, simple, virtual or downloadable) products get assigned to a main product. This then handles the product data for the whole collection (e.g. group, bundle, or configurable product name, price and status).

Downloadable and Bundle products have extra tables in the database, meanwhile the rest are shared amongst all other product types. Configurable products have an extra table to link to child products, catalog_product_super_link.

**Custom Product Type**

To create a product type that extends one of the built-in product types, the corresponding product type model should be extended. Otherwise the new product type should extend the Mage_Catalog_Model_Product_Type_Abstract class.

An entry in the module’s config.xml is also required:

<global>
    <catalog>
        <product>
            <type>
                <{name}>
                    <label></label>
                    <model></model>
                    <composite></composite>
                    <index_priority></index_priority>
                </{name}>
            </type>
        </product>
    </catalog>
</global>
More complicated products may require other customised areas such as price model and index data retriever.


**Price Calculation**


When dealing with a single product, the price is always calculated on the fly. The price EAV attribute is loaded with the product and final price is calculated by the price model, Mage_Catalog_Model_Product_Type_Price.

Some product types deal with it differently. In which case they extend this class and implement their own logic. For example, the configurable product overwrites getFinalPrice() and adds additional logic. This custom model can then be specified in config.xml with a <price_model> tag.

Product collections, however, use the price index to retrieve pre-calculated prices, eliminating the need to calculate it for each product.

Final price can be adjusted by the observers of the catalog_product_get_final_price event. By default, only the Mage_CatalogRule module observes this event.

Another method to override produce price is to simply set it on the product. If the price is set, the product will not recalculate it.

Product tier price is separate from normal price (although taken into account when calculating price). It’s implemented as a list with a customer group and minimum quantity qualifiers for each tier. Tier prices are displayed in a table, using the catalog/product/view/tierprices.phtml template.

Custom product options get processed when calculating final price. Each option has its own price defined, which gets added to the final price.

Group, tier and special prices all get considered at the same time `($priceModel->getBasePrice())` and the smallest one of the three (or four if you include the regular price) is chosen as the base product price.

**Tax**

The Mage_Tax module uses a product’s tax class and whether or not the product price is inclusive or exclusive of tax in order to identify the correct rate to apply.

The following factors are used to calculate tax on products:

 - Product tax class
 - The amount of tax already included
 - Billing and Shipping addresses
 - Customer tax class
 - Store settings

**Layered Navigation**

The classes responsible for rendering the layered navigation are:

 - Mage_Catalog_Block_Layer_View

   -Handles the filters and options

 - Mage_Catalog_Block_Layer_State

  -Controls what is currently being filtered by

To implement layered navigation on attributes with custom source models the Mage_Catalog_Model_Layer_Filter_Abstract::apply() method would need to be overwritten to dictate how the product collection should be filtered.

Layered navigation is rendered by the Mage_Catalog_Block_Layer_View and Mage_Catalog_Block_Layer_State blocks, which use filter blocks for individual filters.

Layered Navigation uses index table for most filters, e.g. price, product attribute index, decimal product index.

**Categories**

*Categories in the Database*

Category hierarchy is managed by storing a category’s parent id. The full hierarchy is shown in the path column (slash separated IDs). There is a special category with parent_id of 0. This is the true root category and each of the other root categories as defined in Magento use this as a shared parent.

To read and manage a category tree from the database two different classes are used depending if flat catalog is enabled, Mage_Catalog_Model_Resource_Category_Tree and Mage_Catalog_Model_Resource_Category_Flat.

The advantage of flat categories is that it is quicker to query. However, it needs to be rebuilt from the EAV tables each time there is a change.

    getChildren()

returns a comma separated string of immediate children IDs

    getAllChildren()

returns a string or array of all children IDs

    getChildrenCategories()

returns a collection of immediate children categories
N.B. If flat catalog is enabled, the only child categories returned will be ones with include_in_menu = 1. In both cases, only active categories are returned.

**Catalog Price Rules**

Catalog price rules apply discounts to products based on the date, product, website and customer group.

When `getFinalPrice()` is called on a product, the event catalog_product_get_final_price is fired. This is observed by Mage_CatalogRule_Model_Observer which will then look for any catalog price rule that applies to the product. If applicable, it then looks at the database price table and writes the price back to the product model as a Varien data field final_price.

Within the database, the catalogrule table describes rules, their conditions and their actions. catalogrule_product contains the matched products and some rule information. Meanwhile catalogrule_product_price contains the price after the rule has been applied.

**Indexing and Flat Tables**

Flat catalog tables are managed by catalog indexers. If automatic rebuilding of the indexes is enabled, the catalog indexers get rebuilt every time a product, category or any related entities are updated. The _afterSave() method calls the indexer process. Otherwise they have to be manually re-indexed through admin.

Product type affects price index and stock index where products can define their own custom indexers (in config.xml) to handle their data for these indexes.

The Mage_Index module provides the framework with which custom indexes can be created to help optimise the performance of the site. The Mage_Index_Model_Indexer_Abstract class should be extended to create a new index, implementing the _registerEvent() and _processEvent() methods. Not forgetting to register it in config.xml:

    <global>
        <index>
            <indexer>
                <{name}>{model}</{name}>
            </indexer>
        </index>
    </global>

## Mage_Catalog_Model_Product_Type
    /**
     * Magento
     *
     * NOTICE OF LICENSE
     *
     * This source file is subject to the Open Software License (OSL 3.0)
     * that is bundled with this package in the file LICENSE.txt.
     * It is also available through the world-wide-web at this URL:
     * http://opensource.org/licenses/osl-3.0.php
     * If you did not receive a copy of the license and are unable to
     * obtain it through the world-wide-web, please send an email
     * to license@magentocommerce.com so we can send you a copy immediately.
     *
     * DISCLAIMER
     *
     * Do not edit or add to this file if you wish to upgrade Magento to newer
     * versions in the future. If you wish to customize Magento for your
     * needs please refer to http://www.magentocommerce.com for more information.
     *
     * @category    Mage
     * @package     Mage_Catalog
     * @copyright   Copyright (c) 2012 Magento Inc. (http://www.magentocommerce.com)
     * @license     http://opensource.org/licenses/osl-3.0.php  Open Software License (OSL 3.0)
     */
    
    /**
     * Product type model
     *
     * @category    Mage
     * @package     Mage_Catalog
     * @author      Magento Core Team 
     */
    class Mage_Catalog_Model_Product_Type
    {
        /**
         * Available product types
         */
        const TYPE_SIMPLE       = 'simple';
        const TYPE_BUNDLE       = 'bundle';
        const TYPE_CONFIGURABLE = 'configurable';
        const TYPE_GROUPED      = 'grouped';
        const TYPE_VIRTUAL      = 'virtual';
    
        const DEFAULT_TYPE      = 'simple';
        const DEFAULT_TYPE_MODEL    = 'catalog/product_type_simple';
        const DEFAULT_PRICE_MODEL   = 'catalog/product_type_price';
    
        static protected $_types;
        static protected $_compositeTypes;
        static protected $_priceModels;
        static protected $_typesPriority;
    
        /**
         * Product type instance factory
         *
         * @param   Mage_Catalog_Model_Product $product
         * @param   bool $singleton
         * @return  Mage_Catalog_Model_Product_Type_Abstract
         */
        public static function factory($product, $singleton = false)
        {
            $types = self::getTypes();
            $typeId = $product->getTypeId();
    
            if (!empty($types[$typeId]['model'])) {
                $typeModelName = $types[$typeId]['model'];
            } else {
                $typeModelName = self::DEFAULT_TYPE_MODEL;
                $typeId = self::DEFAULT_TYPE;
            }
    
            if ($singleton === true) {
                $typeModel = Mage::getSingleton($typeModelName);
            }
            else {
                $typeModel = Mage::getModel($typeModelName);
                $typeModel->setProduct($product);
            }
            $typeModel->setConfig($types[$typeId]);
            return $typeModel;
        }
    
        /**
         * Product type price model factory
         *
         * @param   string $productType
         * @return  Mage_Catalog_Model_Product_Type_Price
         */
        public static function priceFactory($productType)
        {
            if (isset(self::$_priceModels[$productType])) {
                return self::$_priceModels[$productType];
            }
    
            $types = self::getTypes();
    
            if (!empty($types[$productType]['price_model'])) {
                $priceModelName = $types[$productType]['price_model'];
            } else {
                $priceModelName = self::DEFAULT_PRICE_MODEL;
            }
    
            self::$_priceModels[$productType] = Mage::getModel($priceModelName);
            return self::$_priceModels[$productType];
        }
    
        static public function getOptionArray()
        {
            $options = array();
            foreach(self::getTypes() as $typeId=>$type) {
                $options[$typeId] = Mage::helper('catalog')->__($type['label']);
            }
    
            return $options;
        }
    
        static public function getAllOption()
        {
            $options = self::getOptionArray();
            array_unshift($options, array('value'=>'', 'label'=>''));
            return $options;
        }
    
        static public function getAllOptions()
        {
            $res = array();
            $res[] = array('value'=>'', 'label'=>'');
            foreach (self::getOptionArray() as $index => $value) {
                $res[] = array(
                   'value' => $index,
                   'label' => $value
                );
            }
            return $res;
        }
    
        static public function getOptions()
        {
            $res = array();
            foreach (self::getOptionArray() as $index => $value) {
                $res[] = array(
                   'value' => $index,
                   'label' => $value
                );
            }
            return $res;
        }
    
        static public function getOptionText($optionId)
        {
            $options = self::getOptionArray();
            return isset($options[$optionId]) ? $options[$optionId] : null;
        }
    
        static public function getTypes()
        {
            if (is_null(self::$_types)) {
                $productTypes = Mage::getConfig()->getNode('global/catalog/product/type')->asArray();
                foreach ($productTypes as $productKey => $productConfig) {
                    $moduleName = 'catalog';
                    if (isset($productConfig['@']['module'])) {
                        $moduleName = $productConfig['@']['module'];
                    }
                    $translatedLabel = Mage::helper($moduleName)->__($productConfig['label']);
                    $productTypes[$productKey]['label'] = $translatedLabel;
                }
                self::$_types = $productTypes;
            }
    
            return self::$_types;
        }
    
        /**
         * Return composite product type Ids
         *
         * @return array
         */
        static public function getCompositeTypes()
        {
            if (is_null(self::$_compositeTypes)) {
                self::$_compositeTypes = array();
                $types = self::getTypes();
                foreach ($types as $typeId=>$typeInfo) {
                    if (array_key_exists('composite', $typeInfo) && $typeInfo['composite']) {
                        self::$_compositeTypes[] = $typeId;
                    }
                }
            }
            return self::$_compositeTypes;
        }
    
        /**
         * Return product types by type indexing priority
         *
         * @return array
         */
        public static function getTypesByPriority()
        {
            if (is_null(self::$_typesPriority)) {
                self::$_typesPriority = array();
                $a = array();
                $b = array();
    
                $types = self::getTypes();
                foreach ($types as $typeId => $typeInfo) {
                    $priority = isset($typeInfo['index_priority']) ? abs(intval($typeInfo['index_priority'])) : 0;
                    if (!empty($typeInfo['composite'])) {
                        $b[$typeId] = $priority;
                    } else {
                        $a[$typeId] = $priority;
                    }
                }
    
                asort($a, SORT_NUMERIC);
                asort($b, SORT_NUMERIC);
    
                foreach (array_keys($a) as $typeId) {
                    self::$_typesPriority[$typeId] = $types[$typeId];
                }
                foreach (array_keys($b) as $typeId) {
                    self::$_typesPriority[$typeId] = $types[$typeId];
                }
            }
            return self::$_typesPriority;
        }
    }

## Describe standard product types (simple, configurable, bundled)
**Simple**

The Simple Products type should be used for that generally have a single configuration (one-size-fits-all). This might include items such as:

 - A Box of Crayons, Small (24 Colors)
 - A Box of Crayons, Large (64 Colors)
 - SuperHighTech 26” HD Computer Monitor
 - Barrack Obama Action Figure (6”)

**Grouped**

Grouped products allow you to create a new product using one or more existing products in your store. For instance, let’s assume you have a “Barrack Obama Action Figure” and a “George W Bush Action Figure” already in your store and you wanted to sell them as a bundle. You would simply create a new Grouped Product (let’s call it “Obama + Bush (Get Both and Spend Twice as Much!)”, then add both action figures to the group via the “Associated Products” tab.

Note: Unfortunately, you are not able to set a special “group” price directly from the product page. To offer a discount for buying items together, you will need to create a new Shopping Cart Price Rule.

**Configurable**

Configurable Product : This product enables your customers to select the variant that they want by choosing options. For example, you can sell T-shirts in two colors and three sizes. You would create six simple products as individual products (each with its own SKUs) and then add these six to a configurable product where customers can choose the size and color, and then add it to their cart. Very similar functionality is possible by using Custom Options for Simple products. The difference between a configurable product and a product including custom options is that inventory is not checked or updated for individual options during the purchase of the custom options.

**Virtual**

Virtual Products are those that do not have a physical or digital counterpart. They do not ship, nor do they have a download link. This product type might be used for services like:

 - House Cleaning
 - 1-Year Newsletter Subscription

Note: If using Virtual Products for “subscriptions”, it is important to note that there is no built-in way to manage auto-renewing subscriptions. All purchases made in Magento, regardless of Product Type, are one-time purchases.

**Bundle**

This product type is also known as a “kit” in other eCommerce software. This product type is ideal for circumstances where the user have to select a number of configurable options, but at least one option. This might includes products like:

 - Customizable Computer Systems
 - Customizable Tuxedos/Suits
 - Click here for a video tutorial on using bundles

**Downloadable**

Downloadable products are similar to virtual products, except that they include the ability to add one or more digital files for download. Files can either be uploaded via the Admin interface, or by uploading directly to the server via FTP and then added by URL. When a customer buys a Downloadable product, Magento will generate a secure, encrypted link (so that the customers can’t see the file’s real location) for that customer to download their file.

This category might include products such as:

 - Music/MP3s
 - Computer Software

**Note**: If you have SSL enabled for your site, downloads may fail under all versions of IE as IE contains a bug that prevents downloading over secure connections if the no-cache header is set. This can be easily fixed in an htaccess file by removing the no-cache and no-store headers, or by forcing download links to to be non-secure.

