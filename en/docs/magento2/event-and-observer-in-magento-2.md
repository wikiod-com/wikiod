---
title: "Event and observer in magento 2"
slug: "event-and-observer-in-magento-2"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## How to use custom event and observer ?
**Step 1:** Create `events.xml` file according to your requirement in `frontend`, `Backend`, or both `YKM/Banner/etc/frontend/events.xml`

    <?xml version="1.0" encoding="UTF-8"?>
    <config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../../../../../lib/internal/Magento/Framework/Event/etc/events.xsd">
        <event name="controller_action_predispatch">
            <observer name="ykm_banner_before" instance="YKM\Banner\Observer\Help" />
        </event>
    </config>

**Step 2:**

Create an Observer file `YKM/Banner/Observer/Help.php`

    <?php
    /**
     * Copyright © 2015 Magento. All rights reserved.
     * See COPYING.txt for license details.
     */
    namespace Estdevs\Banner\Observer;

    use Magento\Framework\Event\ObserverInterface;

    class Help implements ObserverInterface
    {
        
        public function execute(\Magento\Framework\Event\Observer $observer) {
           echo "this is good.";
        }
    }



