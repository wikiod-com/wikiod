---
title: "Specific urls"
slug: "specific-urls"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Cart url
`$this->helper('checkout/url')->getCartUrl();`

**OR**

`Mage::helper('checkout/url')->getCartUrl();`



## Checkout url
`$this->helper('checkout/url')->getCheckoutUrl();`

**OR**

`Mage::helper('checkout/url')->getCheckoutUrl();`

## Login url
`$this->helper('customer/data')->getLoginUrl();`

**OR**

Mage::helper('customer/data')->getLoginUrl();

## Logout url
`$this->helper('customer/data')->getLogoutUrl();`

**OR**

    Mage::helper('customer/data')->getLogoutUrl();

## Forgot password url
`$this->helper('customer/data')->getForgotPasswordUrl();`

**OR**

`Mage::helper('customer/data')->getForgotPasswordUrl();`

## Account customer url
`$this->helper('customer/data')->getAccountUrl();`

**OR** 

`Mage::helper('customer/data')->getAccountUrl();`

## Media, JS, Skin URL
**To Retrieve URL path in STATIC BLOCK Or CMS pages**

*To get SKIN URL*

    {{skin url=’images/sampleimage.jpg’}}

*To get Media URL*

    {{media url=’/sampleimage.jpg’}}

*To get Store URL*

    {{store url=’mypage.html’}}

*To get Base URL*

    {{base url=”}}

**TO Retrieve URL path in PHTML**

*Not secure Skin URL*

    <?php echo $this->getSkinUrl(‘images/sampleimage.jpg’) ?>

*Secure Skin URL*

    <?php echo $this->getSkinUrl(‘images/ sampleimage.gif’,array(‘_secure’=>true)) ?>

*Get  Current URL*

    <?php $current_url = Mage::helper(‘core/url’)->getCurrentUrl();?>

*Get Home URL*

    <?php $home_url = Mage::helper(‘core/url’)->getHomeUrl();?>

*Get Magento Media Url*

    <?php Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_LINK);?>
    <?php Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_MEDIA);?>

*Get Magento Skin Url*

    <?php Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_SKIN);?>

*Get Magento Store Url*

    <?php Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_WEB);?>

*Get Magento Js Url*

    <?php Mage::getBaseUrl(Mage_Core_Model_Store::URL_TYPE_JS);?>

