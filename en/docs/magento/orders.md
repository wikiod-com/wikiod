---
title: "Orders"
slug: "orders"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Get order by ID
```
$orderid = 12345;
$order = Mage::getModel('sales/order')->load($orderid);
```

The above code is roughly analogous to the following SQL query.

```
select * from sales_flat_order where entity_id=12345;
```

## Get order by Increment ID
```
$incrementid = 100000000;
$order = Mage::getModel('sales/order')->loadByIncrementId($incrementid);
```

The above code is roughly analogous to the following SQL query.

```
select * from sales_flat_order where increment_id=100000000;
```

The `increment_id` is the customer facing order identifier, whereas the `entity_id` is the database level identifier for the order.

## Add comment to order history
You can add comment and status to order.
Get order : 

    $orderid = 12345;
    $order = Mage::getModel('sales/order')->load($orderid);

And add comment:

    //$isNotify means you want to notify customer or not.

    $order->addStatusToHistory($status, $message, $isNotify);
    $order->save()



