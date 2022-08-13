---
title: "Add different price for multiple store using Magento SOAP API"
slug: "add-different-price-for-multiple-store-using-magento-soap-api"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Magento SOAP V1
You need to change price scope **'Global' to 'website'**
(Sysytem->Configuration->Catalog->Catalog->Price)


    $client = new SoapClient('http://your-web-site/api/soap/?wsdl');
    $API_USER = 'your-api-user';
    $API_KEY = 'your-api-key';
    $session = $client->login($API_USER, $API_KEY);
    $result = $client->call($session, 'catalog_product.update', array('test-product', array('price' => '100'),'your-store-code'));
    print "<pre>";
    print_r($result);
    print "</pre>";

