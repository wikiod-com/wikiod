---
title: "Caching"
slug: "caching"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Enabling Hibernate Caching in WildFly
To enable [Second Level Caching][1] for Hibernate in WildFly, add this property to your `persistence.xml` file:

    <property name="hibernate.cache.use_second_level_cache" value="true"/>
           
You may also enable [Query Caching][1] with this property:

    <property name="hibernate.cache.use_query_cache" value="true"/>

WildFly does not require you to define a Cache Provider when enabling Hibernate's Second-Level Cache, as Infinispan is used by default. If you would like to use an alternative Cache Provider, however, you may do so with the `hibernate.cache.provider_class` property.

  [1]: http://www.tutorialspoint.com/hibernate/hibernate_caching.htm

