---
title: "Compile JasperReports .jrxml to .jasper"
slug: "compile-jasperreports-jrxml-to-jasper"
draft: false
images: []
weight: 9743
type: docs
toc: true
---

## With IDE (Integrated development environment)
In IDE [Jaspersoft Studio](http://community.jaspersoft.com/project/jaspersoft-studio) (*JSS*) or the older version [iReport Designer](http://community.jaspersoft.com/project/ireport-designer) it is sufficient to press **Preview**. 

The JasperReports design file [`.jrxml`](https://www.wikiod.com/jasper-reports/getting-started-with-jasper-reports#Jasper report file formats) will automatically be compiled to [`.jasper`](https://www.wikiod.com/jasper-reports/getting-started-with-jasper-reports#Jasper report file formats) in same folder as [`.jrxml`](https://www.wikiod.com/jasper-reports/getting-started-with-jasper-reports#Jasper report file formats) if **no errors** are present.

Another way is to press *"Compile Report"* button in *JSS*

[![Compile via JSS][1]][1]

or use the context menu *"Compile Report"* called from *Report Inspector* in *iReport*

[![Compile via iReport][2]][2]


  [1]: http://i.stack.imgur.com/Ti089.png
  [2]: http://i.stack.imgur.com/QjXbn.png

## With Java
While it is possible to compile `.jrxml` files into `.jasper` files using Java code, this incurs a performance hit that is best avoided by pre-compiling `.jrxml` files using the IDE. With that in mind, compiling `.jrxml` files can be accomplished using the [JasperCompileManager](http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/JasperCompileManager.html) as follows:

    JasperCompileManager.compileReportToFile(
                "designFile.jrxml", //Relative or absoulte path to the .jrxml file to compile
                "compiled.jasper"); //Relative or absolute path to the compiled file .jasper

## With Apache Maven
The [*JasperReports-plugin*](https://github.com/alexnederlof/Jasper-report-maven-plugin) by [Alex Nederlof](https://github.com/alexnederlof) is a good alternative of abandoned [org.codehaus.mojo:jasperreports-maven-plugin](https://mvnrepository.com/artifact/org.codehaus.mojo/jasperreports-maven-plugin) plugin.

The adding of plugin is a typical, simple procedure:

    <build>
        <plugins>
            <plugin>
                <groupId>com.alexnederlof</groupId>
                <artifactId>jasperreports-plugin</artifactId>
                <version>2.3</version>
                <executions>
                    <execution>
                        <phase>process-sources</phase>
                        <goals>
                            <goal>jasper</goal>
                        </goals>
                    </execution>
                </executions>
                <configuration>
                    <sourceDirectory>src/main/resources/jrxml</sourceDirectory>
                    <outputDirectory>${project.build.directory}/jasper</outputDirectory>
                </configuration>
            </plugin>
        </plugins>
    </build>

The command for compilation with *Maven*:

>*mvn jasperreports:jasper*

The [*jasper*](https://www.wikiod.com/jasper-reports/getting-started-with-jasper-reports#Jasper report file formats) files will be created in *${project.build.directory}/jasper* folder (for example, in */target/jasper*)

## With Apache Ant
    <target name="compile" description="Compiles report designs specified using the 'srcdir' in the &lt;jrc&gt; tag." depends="prepare-compile-classpath"> 
      <mkdir dir="./build/reports"/> 
        <taskdef name="jrc" classname="net.sf.jasperreports.ant.JRAntCompileTask"> 
            <classpath refid="project-classpath"/>
        </taskdef>
        <jrc 
                srcdir="./reports"
                destdir="./build/reports"
                tempdir="./build/reports"
                keepjava="true"
                xmlvalidation="true">
            <classpath refid="sample-classpath"/>
            <include name="**/*.jrxml"/>
        </jrc>
    </target>

> Apache Ant build tool needs to be correctly installed on your system

