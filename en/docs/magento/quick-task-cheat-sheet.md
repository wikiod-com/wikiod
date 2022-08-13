---
title: "Quick Task Cheat Sheet"
slug: "quick-task-cheat-sheet"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

https://gist.github.com/arosenhagen/2397824

Many many snippets very helpful

## Get product Stock Qty
<?php
    $id = 52;
    $_product = Mage::getModel('catalog/product')->load($id);

    // or load it by SKU
    // $sku = "microsoftnatural";
    // $_product = Mage::getModel('catalog/product')->loadByAttribute('sku', $sku);

    $stock = Mage::getModel('cataloginventory/stock_item')->loadByProduct($_product);

    print_r($stock->getData());

    echo $stock->getQty();
    echo $stock->getMinQty();
    echo $stock->getMinSaleQty();

