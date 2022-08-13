---
title: "Generate FIXMETODO reports using the taglist-maven-plugin"
slug: "generate-fixmetodo-reports-using-the-taglist-maven-plugin"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This is a small code (xml) snippet to highlight how to use the [taglist-maven-plugin](http://www.mojohaus.org/taglist-maven-plugin/) to generate customized reports (of TODO, FIXME work ...)

## pom.xml to generate a FIXME report
```xml

<build>
    <plugins>
        <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>project-info-reports</artifactId>
                <version>2.9</version>
                <executions>
                    <execution>
                        <goals>
                            <goal>index</goal>
                        </goals>
                        <phase>site</phase>
                    </execution>
                </executions>
            </plugin>
    </plugins>
</build>

<reporting>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-project-info-reports-plugin</artifactId>
                <version>2.9</version>
                <reportSets>
                    <reportSet>
                    <reports>
                        <report>index</report>
                        <report>issue-tracking</report>
                    </reports>
                    </reportSet>
                </reportSets>
            </plugin>
            <plugin>
                <groupId>org.codehaus.mojo</groupId>
                <artifactId>taglist-maven-plugin</artifactId>
                <version>2.4</version>
                <configuration>
                    <tagListOptions>
                        <tagClasse>
                            <displayName>FIXME Work</displayName>
                            <tags>
                                <tag>
                                    <matchString>FIXME</matchString>
                                    <matchType>ignoreCase</matchType>
                                </tag>
                                <tag>
                                    <matchString>@fixme</matchString>
                                    <matchType>ignoreCase</matchType>
                                </tag>
                            </tags>
                        </tagClasse>
                        <tagClasse>
                            <displayName>TODO Work</displayName>
                            <tags>
                                <tag>
                                    <matchString>TODO</matchString>
                                    <matchType>ignoreCase</matchType>
                                </tag>
                                <tag>
                                    <matchString>@todo</matchString>
                                    <matchType>ignoreCase</matchType>
                                </tag>
                            </tags>
                        </tagClasse>
                    </tagListOptions>
                </configuration>
            </plugin>
        </plugins>
    </reporting>
```

Then run 
```sh
mvn clean site:site
```


