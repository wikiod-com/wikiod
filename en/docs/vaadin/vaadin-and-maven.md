---
title: "Vaadin and Maven"
slug: "vaadin-and-maven"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This would be very useful to the Vaadin and Maven community because there is no documentation

## Vaadin Maven Setup
**Common Maven** 

     mvn -B archetype:generate -DarchetypeGroupId=com.vaadin -DarchetypeArtifactId=vaadin-     archetype-application -DarchetypeVersion=7.7.3 -DgroupId=org.test -DartifactId=vaadin-app -Dversion=1.0-SNAPSHOT

**Advanced Maven** 

     mvn archetype:generate \
    -DgroupId=com.mycompany.mycompanyapp \
    -DartifactId=mycompanyapp \
    -Dversion=1.0 \
    -DpackageName=com.mycompany.mycompanyapp \
    -DarchetypeGroupId=com.vaadin \
    -DarchetypeArtifactId=vaadin-archetype-application \
    -DthemeName=mytheme \
    -DuiName=MyCompanyAppUI \
    -DwidgetsetName=MyCompanyAppAppWidgetSet \
    -DarchetypeVersion=LATEST \
    -DinteractiveMode=false

After this is done, run following: `cd ~/mycompanyapp && mvn install -Dmaven.skip.tests=true`


## Pom


 - Repositories


    <repository>
       <id>vaadin-addons</id>
       <url>http://maven.vaadin.com/vaadin-addons</url>
    </repository>
    <repository>
       <id>vaadin-snapshots</id>
       <name>Vaadin snapshot repository</name>
       <url>http://oss.sonatype.org/content/repositories/vaadin-snapshots</url>
            <snapshots>
              <enabled>true</enabled>
            </snapshots>
            <releases>
              <enabled>false</enabled>
            </releases>
    </repository>
    <repository>
        <id>vaadin-releases</id>
        <name>Vaadin releases</name>
        <url>https://oss.sonatype.org/content/repositories/vaadin-releases/</url>
    </repository>`

 - Properties


    <properties>
        <vaadin.version>6.8-SNAPSHOT</vaadin.version>
        <gwt.version>2.3.0</gwt.version>
    </properties>

 - Dependencies


    <dependency>
        <groupId>com.vaadin</groupId>
          <artifactId>vaadin-testbench</artifactId>
          <version>3.0.4</version>
         <scope>test</scope>
        </dependency>
        <dependency>
            <groupId>com.vaadin.addon</groupId>
            <artifactId>vaadin-touchkit-agpl</artifactId>
            <version>2.1.3</version>
            <type>jar</type>
        </dependency>
        <dependency>
            <groupId>org.vaadin.vol</groupId>
            <artifactId>openlayers-wrapper</artifactId>
            <version>1.2.0</version>
        </dependency>
        <dependency>
            <groupId>com.vaadin</groupId>
            <artifactId>vaadin</artifactId>
            <version>${vaadin.version}</version>
        </dependency>
        <dependency>
            <groupId>javax.servlet</groupId>
            <artifactId>servlet-api</artifactId>
            <version>2.3</version>
            <scope>provided</scope>
        </dependency>
        <dependency>
            <groupId>com.google.gwt</groupId>
            <artifactId>gwt-user</artifactId>
            <version>${gwt.version}</version>
            <scope>provided</scope>
        </dependency>
        <dependency>
            <groupId>com.google.gwt</groupId>
            <artifactId>gwt-dev</artifactId>
            <version>${gwt.version}</version>
            <scope>provided</scope>
        </dependency>
        <dependency>
            <!-- jsoup HTML parser library @ http://jsoup.org/ -->
            <groupId>org.jsoup</groupId>
            <artifactId>jsoup</artifactId>
            <version>1.6.3</version>
        </dependency>
        <dependency>
            <groupId>commons-io</groupId>
            <artifactId>commons-io</artifactId>
            <version>2.4</version>
        </dependency>
        <dependency>
            <groupId>org.vaadin.addons</groupId>
            <artifactId>formbinder</artifactId>
            <version>2.0.0</version>
        </dependency>
        <dependency>
            <groupId>org.eclipse.jetty</groupId>
            <artifactId>jetty-servlets</artifactId>
            <version>8.1.7.v20120910</version>
        </dependency>
        <dependency>
            <groupId>junit</groupId>
            <artifactId>junit</artifactId>
            <version>LATEST</version>
            <scope>test</scope>
        </dependency>`
 - Build


   - Plugins


            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <configuration>
                    <source>1.7</source>
                    <target>1.7</target>
                </configuration>
            </plugin>
            <plugin>
                <groupId>org.codehaus.mojo</groupId>
                <artifactId>gwt-maven-plugin</artifactId>
                <version>2.3.0-1</version>
                <configuration>
                    <extraJvmArgs>-Xmx512M -Xss1024k</extraJvmArgs>
                    <!-- <runTarget>mobilemail</runTarget> -->
                    <!-- We are doing "inplace" but into subdir VAADIN/widgetsets. This 
                        way compatible with Vaadin eclipse plugin. -->
                    <webappDirectory>${basedir}/src/main/webapp/VAADIN/widgetsets
                    </webappDirectory>
                    <hostedWebapp>${basedir}/src/main/webapp/VAADIN/widgetsets
                    </hostedWebapp>
                    <noServer>true</noServer>
                    <!-- Remove draftCompile when project is ready -->
                    <draftCompile>false</draftCompile>
                    <compileReport>false</compileReport>
                    <style>OBF</style>
                    <runTarget>http://localhost:8080/</runTarget>
                </configuration>
                <executions>
                    <execution>
                        <goals>
                            <goal>resources</goal>
                            <goal>compile</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
            <!-- As we are doing "inplace" GWT compilatio, ensure the widgetset -->
            <!-- directory is cleaned properly -->
            <plugin>
                <artifactId>maven-clean-plugin</artifactId>
                <version>2.4.1</version>
                <configuration>
                    <filesets>
                        <fileset>
                            <directory>src/main/webapp/VAADIN/widgetsets</directory>
                        </fileset>
                    </filesets>
                </configuration>
            </plugin>

            <plugin>
                <groupId>com.vaadin</groupId>
                <artifactId>vaadin-maven-plugin</artifactId>
                <version>1.0.2</version>
                <executions>
                    <execution>
                        <configuration>
                            <!-- if you don't specify any modules, the plugin will find them -->
                            <!-- <modules> <module>com.vaadin.demo.mobilemail.gwt.ColorPickerWidgetSet</module> 
                                </modules> -->
                        </configuration>
                        <goals>
                            <goal>update-widgetset</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
            <plugin>
                <groupId>org.mortbay.jetty</groupId>
                <artifactId>jetty-maven-plugin</artifactId>
                <version>8.1.6.v20120903</version>
                <configuration>
                    <systemProperties>
                        <systemProperty>
                            <name>jetty.port</name>
                            <value>5678</value>
                        </systemProperty>
                    </systemProperties>
                </configuration>
                <executions>
                    <!-- start and stop jetty (running our app) when running integration 
                        tests -->
                    <execution>
                        <id>start-jetty</id>
                        <phase>pre-integration-test</phase>
                        <goals>
                            <goal>run-exploded</goal>
                        </goals>
                        <configuration>
                            <scanIntervalSeconds>0</scanIntervalSeconds>
                            <daemon>true</daemon>
                            <stopKey>STOP</stopKey>
                            <stopPort>8866</stopPort>
                        </configuration>
                    </execution>
                    <execution>
                        <id>stop-jetty</id>
                        <phase>post-integration-test</phase>
                        <goals>
                            <goal>stop</goal>
                        </goals>
                        <configuration>
                            <stopPort>8866</stopPort>
                            <stopKey>STOP</stopKey>
                        </configuration>
                    </execution>
                </executions>
            </plugin>'









