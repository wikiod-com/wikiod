---
title: "CAC enabling Tomcat for Development Purposes"
slug: "cac-enabling-tomcat-for-development-purposes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Creating the Keystores and configuring Tomcat
This writeup walks though steps to configure Tomcat to request CAC certificates from the client.  It is focused on setting up a development environment, so some features that should be considered for production are not here. (For example it shows using a self-signed certificate for https and it doesn’t consider checking for revoked certificates.)



**Create Keystore for enabling HTTPS connections**

The first step is to set up SSL on tomcat. This is documented on the tomcat website here:  https://tomcat.apache.org/tomcat-8.5-doc/ssl-howto.html for completeness the steps to set it up with a self-signed certificate are listed below:

We need to create a keystore file that holds the SSL certificate for the server. The certificate is what is required to create an https connection and doesn’t have anything to do with making the server request CAC certificates from the client but https connections are required for client certificate authentication.  For a development environment creating a self-signed certificate is ok but it’s discouraged for production.  Java comes packaged with a utility called keytool (http://docs.oracle.com/javase/8/docs/technotes/tools/windows/keytool.html) that is used to managed certificates and keystores. It can be used to create a self signed certificate and add it to a keystore.  To do that you can issue the following command from a command prompt:

>keytool -genkey -alias tomcat -keyalg RSA  -keystore \path\to\my\keystore -storepass changeit

You will be prompted for various bits of information and then a keystore file named “\path\to\my\keystore” with a password of ‘changeit’ will be created and it will contain the generate self-signed certificate.




**Create truststore containing DoD root certificates**

The next thing that is needed is to create a truststore that will contain the DoD root certificates.  The certificates in this truststore will be considered as trusted by tomcat and it will only accept client certificates that have one of the trusted certs in their certificate chain. 

To create the truststore we need to get a copy of the DoD root certificates. To do this download “InstallRoot 5.0” from http://militarycac.com/dodcerts.htm. Install it and run then run it.  Expand the Install DoD Certificates pane and click on the Certificate tab:
[![InstallRoot 5.0 Default Screen][1]][1] 

Next select the three DoD Root CA certs from the list of certificates and click “PEM” under Export tool group:
[![Export DoD Root Certs][2]][2] 

After clicking the “PEM” export button choose a location to export the certificates to and click OK.  This should have created three .cer files in the directory you selected.  Open up a command prompt and navigate to that directory.

Here we will use the keytool command to import the certificates into a truststore.  Run the following commands to import the three certificates:

>keytool -importcert -file DoD_Root_CA_2__0x05__DoD_Root_CA_2.cer -alias DODRoot2 -keystore truststore.jks -storepass changeit

>keytool -importcert -file DoD_Root_CA_3__0x01__DoD_Root_CA_3.cer -alias DODRoot3 -keystore truststore.jks -storepass changeit

>keytool -importcert -file DoD_Root_CA_4__0x01__DoD_Root_CA_4.cer -alias DODRoot4 -keystore truststore.jks -storepass changeit

This will create a truststore.jks file with a password of ‘changeit’ in the current working directory. It will contain the three DoD Root Certs, you can see this by running: 

>keytool -list -keystore truststore.jks

Which should list out something like:

> Your keystore contains 3 entries
> 
> dodroot4, Sep 23, 2016, trustedCertEntry, Certificate fingerprint
> (SHA1): B8:26:9F:25:DB:D9:37:EC:AF:D4:C3:5A:98:38:57:17:23:F2:D0:26
> dodroot3, Sep 23, 2016, trustedCertEntry, Certificate fingerprint
> (SHA1): D7:3C:A9:11:02:A2:20:4A:36:45:9E:D3:22:13:B4:67:D7:CE:97:FB
> dodroot2, Sep 23, 2016, trustedCertEntry, Certificate fingerprint
> (SHA1): 8C:94:1B:34:EA:1E:A6:ED:9A:E2:BC:54:CF:68:72:52:B4:C9:B5:61



  
  
  
  
**Configure Tomcat to use the Keystore and Truststore**  

We now have the keystore and truststore files we need, next is to configure tomcat to use them. To do this we must change the <TomcatHome>/conf/server.xml file.  Open the file in add a connector definition like the following:

    <Connector 
        clientAuth="true" 
        keystoreFile="path/to/keystore.jks" 
        keystorepass="changeit" 
        keystoreType="jks" 
        truststoreFile="path/to/truststore.jks" 
        truststoreType="jks" 
        truststorepass="changeit"
        maxThreads="150" 
        port="8443" 
        protocol="org.apache.coyote.http11.Http11NioProtocol" 
        scheme="https" 
        secure="true" 
        sslProtocol="TLS" 
        SSLEnabled="true" 
        />

You can go here for further definition of all of the attributes: http://tomcat.apache.org/tomcat-7.0-doc/config/http.html

Once this is all done start up tomcat. From a computer that has a CAC reader with a CAC inserted browse to the https://:8443/ url and if everything is configured properly you should be prompted to pick a certificate from the CAC card.

  [1]: http://i.stack.imgur.com/Ol1Y9.jpg
  [2]: http://i.stack.imgur.com/AT7PY.jpg
  

