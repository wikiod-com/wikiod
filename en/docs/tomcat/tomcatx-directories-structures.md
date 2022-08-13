---
title: "Tomcat(x) Directories Structures"
slug: "tomcatx-directories-structures"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Directory Structure in Ubuntu (Linux)
After installing Tomcat with apt-get on Ubuntu xx.xx, Tomcat creates and uses these directories:

$cd /etc/tomcat6/

    ├── Catalina
    │   └── localhost
    │       ├── ROOT.xml
    │       └── solr.xml -> ../../../solr/solr-tomcat.xml
    ├── catalina.properties
    ├── context.xml
    ├── logging.properties
    ├── policy.d
    │   ├── 01system.policy
    │   ├── 02debian.policy
    │   ├── 03catalina.policy
    │   ├── 04webapps.policy
    │   ├── 05solr.policy -> /etc/solr/tomcat.policy
    │   └── 50local.policy
    ├── server.xml
    ├── tomcat-users.xml
    └── web.xml

$cd /usr/share/tomcat6

    ├── bin
    │   ├── bootstrap.jar
    │   ├── catalina.sh
    │   ├── catalina-tasks.xml
    │   ├── digest.sh
    │   ├── setclasspath.sh
    │   ├── shutdown.sh
    │   ├── startup.sh
    │   ├── tomcat-juli.jar -> ../../java/tomcat-juli.jar
    │   ├── tool-wrapper.sh
    │   └── version.sh
    ├── defaults.md5sum
    ├── defaults.template
    └── lib
        ├── annotations-api.jar -> ../../java/annotations-api-6.0.35.jar
        ├── catalina-ant.jar -> ../../java/catalina-ant-6.0.35.jar
        ├── catalina-ha.jar -> ../../java/catalina-ha-6.0.35.jar
        ├── catalina.jar -> ../../java/catalina-6.0.35.jar
        ├── catalina-tribes.jar -> ../../java/catalina-tribes-6.0.35.jar
        ├── commons-dbcp.jar -> ../../java/commons-dbcp.jar
        ├── commons-pool.jar -> ../../java/commons-pool.jar
        ├── el-api.jar -> ../../java/el-api-2.1.jar
        ├── jasper-el.jar -> ../../java/jasper-el-6.0.35.jar
        ├── jasper.jar -> ../../java/jasper-6.0.35.jar
        ├── jasper-jdt.jar -> ../../java/ecj.jar
        ├── jsp-api.jar -> ../../java/jsp-api-2.1.jar
        ├── servlet-api.jar -> ../../java/servlet-api-2.5.jar
        ├── tomcat-coyote.jar -> ../../java/tomcat-coyote-6.0.35.jar
        ├── tomcat-i18n-es.jar -> ../../java/tomcat-i18n-es-6.0.35.jar
        ├── tomcat-i18n-fr.jar -> ../../java/tomcat-i18n-fr-6.0.35.jar
        └── tomcat-i18n-ja.jar -> ../../java/tomcat-i18n-ja-6.0.35.jar


$cd /usr/share/tomcat6-root/

    └── default_root
        ├── index.html
        └── META-INF
            └── context.xml


$cd /usr/share/doc/tomcat6

    ├── changelog.Debian.gz -> ../libtomcat6-java/changelog.Debian.gz
    ├── copyright
    └── README.Debian.gz -> ../tomcat6-common/README.Debian.gz


$cd /var/cache/tomcat6

    ├── Catalina
    │   └── localhost
    │       ├── _
    │       └── solr
    │           └── org
    │               └── apache
    │                   └── jsp
    │                       ├── admin
    │                       │   ├── form_jsp.class
    │                       │   ├── form_jsp.java
    │                       │   ├── get_002dproperties_jsp.class
    │                       │   ├── get_002dproperties_jsp.java
    │                       │   ├── index_jsp.class
    │                       │   ├── index_jsp.java
    │                       │   ├── schema_jsp.class
    │                       │   ├── schema_jsp.java
    │                       │   ├── stats_jsp.class
    │                       │   ├── stats_jsp.java
    │                       │   ├── threaddump_jsp.class
    │                       │   └── threaddump_jsp.java
    │                       ├── index_jsp.class
    │                       └── index_jsp.java
    └── catalina.policy


$cd /var/lib/tomcat6

    ├── common
    │   └── classes
    ├── conf -> /etc/tomcat6
    ├── logs -> ../../log/tomcat6
    ├── server
    │   └── classes
    ├── shared
    │   └── classes
    ├── webapps
    │   └── ROOT
    │       ├── index.html
    │       └── META-INF
    │           └── context.xml
    └── work -> ../../cache/tomcat6

$cd /var/log/tomcat6

    ├── catalina.2013-06-28.log
    ├── catalina.2013-06-30.log
    ├── catalina.out
    ├── catalina.out.1.gz
    └── localhost.2013-06-28.log


$cd /etc/default

    ├── tomcat7

