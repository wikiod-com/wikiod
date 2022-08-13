---
title: "Getting started with ant"
slug: "getting-started-with-ant"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Bootstrap Apache Ivy


## Hello World
Add the following to a file named `build.xml` in your project directory:

    <?xml version="1.0" encoding="UTF-8"?>
    <project name="HelloWorld" default="main">
        <target name="main" description="this is target main">
            <echo message="Hello World" />
        </target>
    </project>

From a command prompt on a computer running Windows, executing `ant main` will display similar to the following:

    $ ant main
    Buildfile: C:\Users\<me>\Projects\HelloWorld\build.xml
    
    main:
         [echo] Hello World
    
    BUILD SUCCESSFUL

Also, user can now run the command `ant` as `default` target name added to the project. When `ant` command is run, it looks for project's `default` target and execute it.

    $ ant 
    Buildfile: C:\Users\<me>\Projects\HelloWorld\build.xml
    
    main:
         [echo] Hello World
    
    BUILD SUCCESSFUL

If the build script is written by some one else and the end user like to see what target he can run, run the command which will show all the targets which has descriptions.

    $ ant -p

## Print environment information before build


## Run JUnit


## Create jar package
The following will create `dist/output.jar` from the source code in `src` and the libraries in `lib`, and will use `src/Main.java` as the main class.

    <project name="Project" default="main" basedir=".">

        <property name="src.dir"     value="src"/>
        <property name="build.dir"   value="build"/>
        <property name="dist.dir"    value="dist"/>

        <path id="classpath">
            <fileset dir="lib" includes="**/*.jar"/>
            <pathelement path="${build.dir}"/>
        </path>

        <target name="clean">
          <delete dir="${build.dir}"/>
          <delete dir="${dist.dir}"/>
        </target>
        

        <target name="compile">
            <mkdir dir="${build.dir}"/>
            <javac srcdir="${src.dir}" destdir="${build.dir}" classpathref="classpath"/>
            <copy todir="${build.dir}">
                <fileset dir="${src.dir}" excludes="**/*.java"/>
            </copy>
        </target>

        <target name="jar" depends="compile">
            <mkdir dir="${dist.dir}"/>
            <jar destfile="${dist.dir}/${ant.project.name}.jar" basedir="${build.dir}">
                <fileset dir="${build.dir}"/>
                <restrict>
                    <archives>
                        <zips>
                            <fileset dir="lib" includes="**/*.jar"/>
                        </zips>
                    </archives>
                </restrict>
                <manifest>
                    <attribute name="Main-Class" value="Main"/>
                </manifest>
            </jar>
        </target>

        <target name="main" depends="clean,jar"/>
    </project>

## Installation or Setup
Installing Ant is very simple. Follow the steps given below to install Ant on windows platform:

 1. Download latest ant version from [Apache website][1] 
 2. Unzip the file on your machine.
 3. Set ANT_HOME in environment variables
 4. Add %ANT_HOME%\bin to your PATH environment variable.
 5. Set CLASSPATH=%ANT_HOME%\lib;%CLASSPATH%
 6. Now open command prompt and enter `ant` command. You should see below:

    <pre>Buildfile: build.xml does not exist!
    Build failed</pre>

Alternatively, using Homebrew on macOS or Linuxbrew on Linux you can simply run:
`brew install ant`

When using brew it isn't necessary to set up the environment variables.

Several Linux distributions also support installing Ant from their respective package managers.

To test Ant is installed properly navigate to the command prompt and execute 

    ant -version

This command will print the Ant version and also shows that Ant was successfully installed. 

Ant's own installation instructions page is available on the [Apache Ant website][2].
 

  [1]: http://ant.apache.org/bindownload.cgi
  [2]: https://ant.apache.org/manual/install.html

