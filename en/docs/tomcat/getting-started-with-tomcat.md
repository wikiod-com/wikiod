---
title: "Getting started with tomcat"
slug: "getting-started-with-tomcat"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting tomcat set up or installed.

## Installing Tomcat as a service on Ubuntu
This example demonstrates how to install Tomcat as a service on Ubuntu using the **.tar.gz* releases of both Tomcat as well as Java.

**1. Install the Java Runtime Environment (JRE)**
-
<ol>
<li>Download the desired jre <em>.tar.gz</em> release</li>
<li>Extract to <code>/opt/</code> <br/>
This will create a directory <code>/opt/jre1.Xxxx/</code></li>
<li>Create a symbolic link to the java home directory:<br/>
<code>cd /opt; sudo ln -s jre1.Xxxxx java</code>
</li>
<li>add the JRE to the JAVA_HOME environment variable:<br/>
<code>sudo vim /etc/environment</code><br/>
<code>JAVA_HOME="/opt/java"</code>
</li>
</ol>

**2. Install Tomcat:**
-
<ol>
<li>Download tomcat in a <em>.tar.gz</em> (or similiar) release.</li>

<li>Create a tomcat system user:<br/>
<code>sudo useradd -r tomcat</code></li>

<li>Extract to <code>/opt/</code><br/>
This will create a directory <code>/opt/apache-tomcat-XXXX</code><br/>
assign this directory to the tomcat system user and group:<br/>
<code>sudo chown -R tomcat ./*</code><br/>
<code>sudo chgrp -R tomcat ./*</code></li>

<li>Create the <code>CATALINA_HOME</code> environment variable:<br/>
<code>sudo vim /etc/environment</code><br/>
<code>CATALINA_HOME="/opt/tomcat"</code></li>

<li>Add admin user in <code>tomcat-users.xml</code><br/>
<code>sudo vim /opt/tomcat/conf/tomcat-users.xml</code><br/>
and add something like
<code>&ltuser username="admin" password="adminpw" roles="manager-gui"&gt</code><br/>
between the <code>&lttomcat-users&gt</code> ... <code>&lt/tomcat-users&gt</code> tags</li>

</ol>

**3. Making Tomcat boot at startup**
-
Add a script in /etc/init.d called tomcat and make it executable.
The content of the script can look something like:

    RETVAL=$?
    CATALINA_HOME="/opt/tomcat"
    
    case "$1" in
     start)
        if [ -f $CATALINA_HOME/bin/startup.sh ];
          then
            echo $"Starting Tomcat"
            sudo -u tomcat $CATALINA_HOME/bin/startup.sh
        fi
        ;;
     stop)
        if [ -f $CATALINA_HOME/bin/shutdown.sh ];
          then
            echo $"Stopping Tomcat"
            sudo -u tomcat $CATALINA_HOME/bin/shutdown.sh
        fi
        ;;
     *)
        echo $"Usage: $0 {start|stop}"
        exit 1
        ;;
    esac

    exit $RETVAL

To make it start on boot, run: `sudo update-rc.d tomcat defaults`

You can also add a bash line to /etc/rc.local for example `service tomcat start`

**Changing classpath or other Tomcat related environment variables:**
-
Edit the file `$CATALINA_HOME/bin/setenv.sh` and add the properties in here, for example: `CLASSPATH=/additional/class/directories`

