---
title: "Access Maven informations in code"
slug: "access-maven-informations-in-code"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

It is sometimes useful to get the maven properties, such as the current version, in code. Here are some ways to to it.

## Getting the version number from within a jar
If you package your application in a `jar` using the `maven-jar-plugin` or the `maven-assembly-plugin`, an easy way to get the current pom version is to add an entry in the manifest, which is then available from Java.

The secret is to set the `addDefaultImplementationEntries` flag to true (and the `addDefaultSpecificationEntries` is you also need the artifact id).
    
__jar plugin configuration__: 

<!-- language: lang-xml -->

    <build>
      <plugins>
        <plugin>
          <groupId>org.apache.maven.plugins</groupId>
          <artifactId>maven-jar-plugin</artifactId>
          <configuration>
            <archive>
              <manifest>
                <mainClass>...</mainClass>
                <addDefaultImplementationEntries>
                  true
                </addDefaultImplementationEntries>
              </manifest>
            </archive>
          </configuration>
        </plugin>
      </plugins>
    </build>

__assembly plugin configuration__: 

    <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-assembly-plugin</artifactId>
        <configuration>
            <descriptorRefs>
                <descriptorRef>jar-with-dependencies</descriptorRef>
            </descriptorRefs>
            <archive>
                <manifest>  
                  <addDefaultImplementationEntries>true</addDefaultImplementationEntries> 
                </manifest>
            </archive>
        </configuration>
        <executions>
            <execution .../>
        </executions>
    </plugin>


`addDefaultImplementationEntries` instructs Maven to add the following headers to the `MANIFEST.MF` of your jar:

    Implementation-Title: display-version
    Implementation-Version: 1.0-SNAPSHOT
    Implementation-Vendor-Id: test

Now you can use this line of code anywhere in your jar to access the version number:

    getClass().getPackage().getImplementationVersion()


More information [here](http://stackoverflow.com/a/6773868/2667536) and [here](http://stackoverflow.com/a/43383222/2667536).

## Keeping a properties file in sync using maven's property filtering mecanism
As [this documentation](http://maven.apache.org/guides/getting-started/index.html#How_do_I_filter_resource_files) explains, 

> Sometimes a resource file will need to contain a value that can only be supplied at build time. To accomplish this in Maven, put a reference to the property that will contain the value into your resource file using the syntax `${<property name>}`. The property can be one of the values defined in your `pom.xml`, a value defined in the user's `settings.xml`, a property defined in an external properties file, or a system property.

As an example, let's create a simple `info.txt` in `src/main/resources` containing the pom version and the build time. 

1. create a `src/main/resources/info.txt` with the following content:

    version=${pom.version}
    build.date=${timestamp}

2. ask Maven to _expand_ the properties by setting `filtering` to true:

    <!-- language: lang-xml -->
        <build>
            <resources>
                <resource>
                   <directory>src/main/resources</directory>
                   <filtering>true</filtering>
                </resource>
            </resources>
        </build>

3. with that, the version will be updated, but unfortunately a bug within Maven prevents the `${maven.build.timestamp}` property from getting passed to the resource filtering mechanism (more info [here](https://dzone.com/articles/stamping-version-number-and)). So, let's create a `timestamp` property as a workaround ! Add the following to the pom's properties:

    <!-- language: lang-xml -->
        <properties>
            <timestamp>${maven.build.timestamp}</timestamp>
            <maven.build.timestamp.format>yyyy-MM-dd'T'HH:mm</maven.build.timestamp.format>  
        </properties>

4. run maven, you should find a `info.txt` in `target/classes` with a content like:

        version=0.3.2
        build.date=2017-04-20T13:56

## Reading a pom.xml at runtime using maven-model plugin
The other examples may be the best and most stable way to get a version number into an application **_statically_**. [This answer](http://stackoverflow.com/a/41791885/2667536) proposes an alternative showing how to do it **_dynamically_** during runtime, using the maven _maven-model_ library.

Add the dependency:

<!-- language: xml -->

    <dependency>
      <groupId>org.apache.maven</groupId>
      <artifactId>maven-model</artifactId>
      <version>3.3.9</version>
    </dependency>

In Java, create a `MavenXpp3Reader` to read your pom. For example:

<!-- language: java -->
    
    package de.scrum_master.app;

    import org.apache.maven.model.Model;
    import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
    import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
    
    import java.io.FileReader;
    import java.io.IOException;
    
    public class MavenModelExample {
        public static void main(String[] args) throws IOException, XmlPullParserException {
            MavenXpp3Reader reader = new MavenXpp3Reader();
            Model model = reader.read(new FileReader("pom.xml"));
            System.out.println(model.getId());
            System.out.println(model.getGroupId());
            System.out.println(model.getArtifactId());
            System.out.println(model.getVersion());
        }
    }

The console log is as follows:

<!-- language: none -->

    de.scrum-master.stackoverflow:my-artifact:jar:1.0-SNAPSHOT
    de.scrum-master.stackoverflow
    my-artifact
    1.0-SNAPSHOT

