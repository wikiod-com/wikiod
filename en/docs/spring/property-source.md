---
title: "Property Source"
slug: "property-source"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Annotation
Sample property file : nexus.properties

Sample property file content:

    nexus.user=admin
    nexus.pass=admin
    nexus.rest.uri=http://xxx.xxx.xxx.xxx:xxxx/nexus/service/local/artifact/maven/content

Sample Context File xml configuration

    <context:property-placeholder location="classpath:ReleaseBundle.properties" />

Sample Property Bean using annotations

    @Component
    @PropertySource(value = { "classpath:nexus.properties" })
    public class NexusBean {
    
        @Value("${" + NexusConstants.NEXUS_USER + "}")
        private String user;
    
        @Value("${" + NexusConstants.NEXUS_PASS + "}")
        private String pass;
    
        @Value("${" + NexusConstants.NEXUS_REST_URI + "}")
        private String restUri;
    }

Sample Constant class

    public class NexusConstants {
        public static final String NexusConstants.NEXUS_USER="";
        public static final String NexusConstants.NEXUS_PASS="";
        public static final String NexusConstants.NEXUS_REST_URI="";
    }

## Sample xml configuration using PropertyPlaceholderConfigurer

    <bean class="org.springframework.beans.factory.config.PropertyPlaceholderConfigurer">
        <property name="locations">
         <list>
               <value>classpath:ReleaseBundle.properties</value>
         </list>
    </bean>

