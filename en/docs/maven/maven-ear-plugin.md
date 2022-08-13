---
title: "Maven EAR plugin"
slug: "maven-ear-plugin"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Here is an example configuration for a basic maven ear plugin for packaging both .war and .jar artifacts

## A basic EAR configuration
```xml
<dependencies>
    <dependency>
        <groupId>{ejbModuleGroupId}</groupId>
        <artifactId>{ejbModuleArtifactId}</artifactId>
        <version>{ejbModuleVersion}</version>
        <type>ejb</type>
    </dependency>
    <dependency>
        <groupId>{webModuleGroupId}</groupId>
        <artifactId>{webModuleArtifactId}</artifactId>
        <version>{webModuleVersion}</version>
        <type>war</type>
    </dependency>
</depencencies>
<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-ear-plugin</artifactId>
                <version>2.9.1</version>
                <configuration>
                    <version>1.4</version><!-- application.xml verion -->
                    <modules>
                        <ejbModule>
                            <groupId>{ejbModuleGroupId}</groupId>
                            <artifactId>{ejbModuleArtifactId}</artifactId>
                        </ejbModule>
                        <webModule>
                            <groupId>{webModuleGroupId}</groupId>
                            <artifactId>{webModuleArtifactId}</artifactId>
                            <contextRoot>/custom-context-root</contextRoot>
                        </webModule>
                    </modules>
                </configuration>
        </plugin>
    </plugins>
</build>
```

Once compiled `mvn clean install`, generates an _.ear_ artifact in the _target_ directory containing both the ejb (.jar) and the web module, along with the _application.xml_ JEE description file.

