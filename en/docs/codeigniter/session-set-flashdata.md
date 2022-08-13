---
title: "session set flashdata"
slug: "session-set-flashdata"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## How to Display Flashdata in view
You can simply access the fashdata in view like this

    <?php echo $this->session->flashdata('message'); ?>
For access multiple message just change identifier

 **For Ex.** 

    <?php echo $this->session->flashdata('my_alert'); ?>
    <?php echo $this->session->flashdata('my_warnig'); ?>

## How to Set session flash data in controller
You can set flash data in controller just using this syntax

    $this->session->set_flashdata('message', 'Message you want to set');
Here 'message' is identifier for access data in view. You can Set more than one message by just changing identifier.

**for ex**

    $this->session->set_flashdata('my_alert', 'Message you want to set');
    $this->session->set_flashdata('my_warnig', 'Message you want to set');

