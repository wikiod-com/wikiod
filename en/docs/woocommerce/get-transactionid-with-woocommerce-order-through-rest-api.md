---
title: "Get transaction_id with woocommerce order through REST API"
slug: "get-transaction_id-with-woocommerce-order-through-rest-api"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## The WordPress woocommerce api response order hook.
    /*add this code to your function.php file
      now your api will include transaction_id
    */

    add_action( 'woocommerce_api_order_response', 'my_woocommerce_api_order', 10, 2);

    function my_woocommerce_api_order( $data ) {

        //you can do anything with the $data here lets add the transaction id
        $data['transaction_id'] = get_post_meta( $data['id'], '_transaction_id', true );          
        
        return $data;
    }

