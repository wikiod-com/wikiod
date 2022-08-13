---
title: "Maven Tomcat Plugin"
slug: "maven-tomcat-plugin"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Start tomcat using maven plugin.
In the example we will start tomcat 7 using maven plugin, optionally add user/password protection for REST end point. Also adding feature of building war.

Add below section in plugin section of pom for tomcat
```
             <plugin>
                <groupId>org.apache.tomcat.maven</groupId>
                <artifactId>tomcat7-maven-plugin</artifactId>
                <version>2.2</version>
                <configuration>
                    <url>http://localhost:8090/manager</url>
                    <server>localhost</server>
                    <port>8191</port>
                    <path>/${project.build.finalName}</path>
                    <tomcatUsers>src/main/tomcatconf/tomcat-users.xml</tomcatUsers>
                </configuration>
            </plugin>
```
Ensure maven war plugin is added and web.xml is present at location
/src/main/webapp/WEB-INF/web.xml. Below is example of war plugin.

```
<plugin>
            <artifactId>maven-war-plugin</artifactId>
            <version>2.3</version>
            </plugin>
            <plugin>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.1</version>
                <configuration>
                    <source>1.7</source>
                    <target>1.7</target>
                    <webResources>
                        <resource>
                            <!-- this is relative to the pom.xml directory -->
                            <directory>/src/main/webapp/WEB-INF/web.xml</directory>
                        </resource>
                    </webResources>
                </configuration>
            </plugin>
```
Optionally, add tomcat-users.xml to location src/main/tomcatconf. It will be copied automatically when tomcat will start.
```
<tomcat-users>
    <user name="user" password="password" roles="admin" />
</tomcat-users>
```
Optionally, add below entry in web.xml to protect REST url.
```
    <!-- tomcat user -->
    <security-constraint>
        <web-resource-collection>
            <web-resource-name>Wildcard means whole app requires authentication</web-resource-name>
            <url-pattern>/helloworld/*</url-pattern>
            <http-method>GET</http-method>
        </web-resource-collection>
        <auth-constraint>
            <role-name>admin</role-name>
        </auth-constraint>
        <user-data-constraint>
            <transport-guarantee>NONE</transport-guarantee>
        </user-data-constraint>
    </security-constraint>
    <login-config>
        <auth-method>BASIC</auth-method>
    </login-config>
```

Create new maven build from eclipse. Select the war project and in the Goals section add below command.
```
tomcat7:run
```

you will see message.

[INFO] --- tomcat7-maven-plugin:2.2:run (default-cli) @ web-service-ldap2 ---
[INFO] Running war on http://localhost:8191/<artifactid-version>


