---
title: "Spring profile"
slug: "spring-profile"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Spring Profiles allows to configure parts available for certain environment
Any `@Component` or `@Configuration` could be marked with `@Profile` annotation

    @Configuration
    @Profile("production")
    public class ProductionConfiguration {
    
        // ...
    }

The same in XML config

    <beans profile="dev">
        <bean id="dataSource" class="<some data source class>" />
    </beans>

Active profiles could be configured in the `application.properties` file

    spring.profiles.active=dev,production

or specified from command line

    --spring.profiles.active=dev,hsqldb

or in SpringBoot

    SpringApplication.setAdditionalProfiles("dev");

It is possible to enable profiles in Tests using the annotation `@ActiveProfiles("dev")`

