---
title: "Getting started with mule"
slug: "getting-started-with-mule"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Mule flow xml for simple hello example
        <?xml version="1.0" encoding="UTF-8"?>
        
        <mule xmlns:http="http://www.mulesoft.org/schema/mule/http" xmlns="http://www.mulesoft.org/schema/mule/core"
            xmlns:doc="http://www.mulesoft.org/schema/mule/documentation"
            xmlns:spring="http://www.springframework.org/schema/beans" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-current.xsd
        http://www.mulesoft.org/schema/mule/core http://www.mulesoft.org/schema/mule/core/current/mule.xsd
        http://www.mulesoft.org/schema/mule/http http://www.mulesoft.org/schema/mule/http/current/mule-http.xsd">
            <http:listener-config name="HTTP_Listener_Configuration"
                host="0.0.0.0" port="8082" doc:name="HTTP Listener Configuration" />
            <flow name="helloworldFlow">
                <http:listener config-ref="HTTP_Listener_Configuration"
                    path="/Hello" allowedMethods="GET" doc:name="HTTP" />
                <set-payload value="Hello #[message.inboundProperties.'http.query.params'.name]" doc:name="Set Payload" />
                <logger message="#[message.payloadAs(java.lang.String)]" level="INFO" doc:name="Logger" />
            </flow>
        </mule>
        

## Basic example to access a database and select all records in the database using anypoint studio
<?xml version="1.0" encoding="UTF-8"?>

<mule xmlns:context="http://www.springframework.org/schema/context" xmlns:db="http://www.mulesoft.org/schema/mule/db" xmlns:json="http://www.mulesoft.org/schema/mule/json" xmlns:http="http://www.mulesoft.org/schema/mule/http" xmlns:tracking="http://www.mulesoft.org/schema/mule/ee/tracking" xmlns="http://www.mulesoft.org/schema/mule/core" xmlns:doc="http://www.mulesoft.org/schema/mule/documentation"
    xmlns:spring="http://www.springframework.org/schema/beans" 
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-current.xsd
http://www.mulesoft.org/schema/mule/core http://www.mulesoft.org/schema/mule/core/current/mule.xsd
http://www.mulesoft.org/schema/mule/db http://www.mulesoft.org/schema/mule/db/current/mule-db.xsd
http://www.mulesoft.org/schema/mule/json http://www.mulesoft.org/schema/mule/json/current/mule-json.xsd
http://www.mulesoft.org/schema/mule/http http://www.mulesoft.org/schema/mule/http/current/mule-http.xsd
http://www.mulesoft.org/schema/mule/ee/tracking http://www.mulesoft.org/schema/mule/ee/tracking/current/mule-tracking-ee.xsd
http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context-current.xsd">


    <http:listener-config name="HTTP_Listener_Configuration" host="localhost" port="${http.port}" doc:name="HTTP Listener Configuration"/>

    <db:mysql-config name="MySQL_Configuration" host="${db.host}" port="${db.port}" user="${db.user}" password="${db.password}" database="${db.database}" doc:name="MySQL Configuration"/>

    <context:property-placeholder location="prop.properties"/>

    <flow name="Total">
        <http:listener config-ref="HTTP_Listener_Configuration" path="/" allowedMethods="GET" doc:name="HTTP"/>

        <db:select config-ref="MySQL_Configuration" doc:name="Database">
            <db:parameterized-query><![CDATA[SELECT * FROM TableName]]></db:parameterized-query>
        </db:select>

        <json:object-to-json-transformer doc:name="Total"/>

    </flow>https://www.wikiod.com/mule/getting-started-with-mule#Mule flow xml for simple hello example
</mule>

## Installation or Setup on MS windows OS
Detailed instructions on getting mule set up or installed.

1. Before going to start with mule we have to insure that java home is set.
2. Mule CE runtime don't need installation.
3. We have to just unzip the downloaded file and go to bin directory of mule runtime.
4. In MS windows Operating system we have to run mule.bat file with admin privilege.
5. Mule will deploy default app and up now.
6. Now you can manually deploy mule app by just past mule app zip file at app directory of runtime and check log in log directory.  
 

