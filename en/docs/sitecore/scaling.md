---
title: "Scaling"
slug: "scaling"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Sitecore out of the box supports load-balancing for multiple server. Typical configuration is Content Management (CM) and Content Delivery (CD) server, however multiple CM and CD servers are supported too.

## Implement remote events in load-balanced enviornment
If Sitecore is setup in a CM-CD enviornment there could be a need to fire events on CD server when CM events are fired.

The example could be firing publish:end:remote on CD when content editors done publish on CM.

In order to make sure that events are firing the following steps are required to be done:

1) Check that event queues are enabled in web.config

<!-- language: xml -->
        <!--  ENABLE EVENT QUEUES
                If enabled, Sitecore sends local events to the event queue available to remote instances,
                and handles events in the queue from remote instances.
                Default value: true
          -->
          <setting name="EnableEventQueues" value="true" />

2) Add ScalabilitySettings.config to each instance. Set InstanceName for each server and PublishingInstance to the CM server instance name.

The example of ScalabilitySettings.config could be found in the App_Config/Include folder.

<!-- language: xml -->
    <!--  INSTANCE NAME
                Unique name for Sitecore instance.
                Default value: (machine name and IIS site name)
          -->
          <setting name="InstanceName">
            <patch:attribute name="value">BAYERUATCD</patch:attribute>
          </setting>
          <!--  PUBLISHING INSTANCE
                Assigns the instance name of dedicated Sitecore installation for publishing operations.
                When empty, all publishing operations are performed on the local installation of Sitecore.
                Default vaue: (empty)
          -->
          <setting name="Publishing.PublishingInstance">
            <patch:attribute name="value">BAYERUATCM</patch:attribute>
          </setting>
          <!--  COUNTERS INSTANCE NAME
                Instance name for performance counters.
                Default value: (value of InstanceName setting)
          -->
          <setting name="Counters.InstanceName">
            <patch:attribute name="value">BAYERUATCD</patch:attribute>
          </setting>
          <!--  SECURITY CACHE EXPIRATION
                Sets the absolute expiration on the cached security data.
                A value of 00:00:00 disables automatic expiration of security caches.
    
          -->




