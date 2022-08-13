---
title: "Setting up SSL"
slug: "setting-up-ssl"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Make sure you have a valid ssl certificate provided by a third party. You can also use a selfsigned certificate, but for dev only.
[Letsencrypt][1] provides free certificates that can be used in production....


  [1]: https://letsencrypt.com

Use keytool to import the certificate to the keystorechain of java.

## How to enable SSL on Tomcat and Liferay
Make sure your tomcat configurations file, server.xml has this line:

    <Connector port="8443" protocol="org.apache.coyote.http11.Http11Protocol"
                  maxHttpHeaderSize="8192" SSLEnabled="true"
                  maxThreads="150" minSpareThreads="25" maxSpareThreads="75"
                  enableLookups="false" disableUploadTimeout="true"
                  acceptCount="100" scheme="https" secure="true"
                  clientAuth="false" useBodyEncodingForURI="true"
                  sslEnabledProtocols="TLSv1.2"
                  keystorePass="passwordtokeystore"
                  keystoreFile="/path/to/.keystoreChain"
                  truststoreFile="%JAVA_HOME%/jdk1.8.0_91/jre/lib/security/cacerts"
                  />

Its important to choose the right sslprotocols, you can add more sslprotocols with a comma seperation inbetween the ssl protocols like this:

    sslEnabledProtocols="TLSv1, TLSv1.1, TLSv1.2"

Then make sure that your portal-ext.properties file in Liferay have this configuration lines:

    web.server.protocol=https


