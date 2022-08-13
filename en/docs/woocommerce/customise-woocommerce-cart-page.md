---
title: "Customise WooCommerce Cart Page"
slug: "customise-woocommerce-cart-page"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Set a custom add to cart URL to redirect to
    /**
    * Set a custom add to cart URL to redirect to
     * @return string
     */
    function custom_add_to_cart_redirect() { 
        return 'http://www.yourdomain.com/your-page/'; 
    }
    add_filter( 'woocommerce_add_to_cart_redirect', 'custom_add_to_cart_redirect' );

## Remove Billing Detail Fields from WooCommerce Checkout Page
Default WooCommerce checkout form comes with several fields for customers to enter their billing details. But in some cases, you might want to hide  some of these fields. For example, if you are selling only virtual products, you can get rid of fields like billing address. 

Use the following code snippet to remove or hide fields from WooCommerce checkout page.

    //Remove Billing Details Fields from WooCommerce Checkout Page
    add_filter( 'woocommerce_checkout_fields' , 'custom_override_checkout_fields' );
     
    function custom_override_checkout_fields( $fields ) {
        unset($fields['billing']['billing_first_name']);
        unset($fields['billing']['billing_last_name']);
        unset($fields['billing']['billing_company']);
        unset($fields['billing']['billing_address_1']);
        unset($fields['billing']['billing_address_2']);
        unset($fields['billing']['billing_city']);
        unset($fields['billing']['billing_postcode']);
        unset($fields['billing']['billing_country']);
        unset($fields['billing']['billing_state']);
        unset($fields['billing']['billing_phone']);
        unset($fields['order']['order_comments']);
        unset($fields['billing']['billing_email']);
        unset($fields['account']['account_username']);
        unset($fields['account']['account_password']);
        unset($fields['account']['account_password-2']);
        return $fields;
    }

