---
title: "Embedding into an application"
slug: "embedding-into-an-application"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Embed tomcat using maven
          <plugin>
                <groupId>org.apache.tomcat.maven</groupId>
                <artifactId>tomcat7-maven-plugin</artifactId>
                <version>2.1</version>
                <executions>
                    <execution>
                        <id>tomcat-run</id>
                          <goals>
                            <goal>exec-war-only</goal>
                         </goals>
    <!--This phase is for creating jar file.You can customize configuration -->
                            <phase>package</phase>
                        <configuration>
                            <path>/WebAppName</path>
                            <enableNaming>false</enableNaming>
                            <finalName>WebAppName.jar</finalName>
                        </configuration>
                    </execution>
                </executions>
    <!--This configuration is for running application in your ide-->
                <configuration>
                    <port>8020</port>
                    <path>/webappName</path>
            <!--These properties are optional-->
                    <systemProperties>
                        <CATALINA_OPTS>-Djava.awt.headless=true -Dfile.encoding=UTF-8
                            -server -Xms1536m -Xmx1536m
                            -XX:NewSize=256m -XX:MaxNewSize=256m -XX:PermSize=256m
                            -XX:MaxPermSize=512m -XX:+DisableExplicitGC
                            -XX:+UseConcMarkSweepGC
                            -XX:+CMSIncrementalMode
                            -XX:+CMSIncrementalPacing
                            -XX:CMSIncrementalDutyCycleMin=0
                            -XX:-TraceClassUnloading
                        </CATALINA_OPTS>
                    </systemProperties>
                </configuration>
            </plugin>

You can run the above tomcat in your ide using goal `tomcat:run`. If you run  `package` goal it will create a jar file in your target folder which can create tomcat instance itself and run.

   Using `</CATALINA_OPTS>` you can specify properties like permgen max and min size, Garbage Collection mechanism etc.which are completely optional. 

