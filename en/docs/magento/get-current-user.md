---
title: "Get current User"
slug: "get-current-user"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Get current Customer
    Mage::helper('customer')->getCustomer();
    
or

    Mage::getSingleton('customer/session')->getCustomer();

## Get current Admin User
    Mage::getSingleton('admin/session')->getUser();

## Check if user is logged in

    Mage::getSingleton('customer/session')->isLoggedIn()

