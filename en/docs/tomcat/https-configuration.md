---
title: "Https configuration"
slug: "https-configuration"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## SSL/TLS Configuration
**HTTPS**

 HTTPS (also called HTTP over TLS,[1][2] HTTP over SSL,[3] and HTTP Secure[4][5]) is a protocol for secure communication over a computer network which is widely used on the Internet. HTTPS consists of communication over Hypertext Transfer Protocol (HTTP) within a connection encrypted by Transport Layer Security or its predecessor, Secure Sockets Layer. The main motivation for HTTPS is authentication of the visited website and protection of the privacy and integrity of the exchanged data.

**SSL** 

Image result for what is ssl
SSL (Secure Sockets Layer) is the standard security technology for establishing an encrypted link between a web server and a browser. This link ensures that all data passed between the web server and browsers remain private and integral.
SSL is a security protocol. Protocols describe how algorithms should be used.


**TLS**

Transport Layer Security (TLS) and its predecessor, Secure Sockets Layer (SSL), both of which are frequently referred to as 'SSL', are cryptographic protocols designed to provide communications security over a computer network.

 **SSL Certificate**

All browsers have the capability to interact with secured web servers using the SSL protocol. However, the browser and the server need what is called an SSL Certificate to be able to establish a secure connection.

SSL Certificates have a key pair: a public and a private key. These keys work together to establish an encrypted connection. The certificate also contains what is called the “subject,” which is the identity of the certificate/website owner.


**How Does the SSL Certificate Create a Secure Connection**

1. When a browser attempts to access a website that is secured by SSL, the browser and the web server establish an SSL connection using a process called an “SSL Handshake”

2. Essentially, three keys are used to set up the SSL connection: the public, private, and session keys.

 **Steps to Establish a Secure Connection**

1. Browser connects to a web server (website) secured with SSL (https). Browser requests that the server identify itself.

2. Server sends a copy of its SSL Certificate, including the server’s public key.

3. Browser checks the certificate root against a list of trusted CAs and that the certificate is unexpired, unrevoked, and that its common name is valid for the website that it is connecting to. If the browser trusts the certificate, it creates, encrypts, and sends back a symmetric session key using the server’s public key.

4. Server decrypts the symmetric session key using its private key and sends back an acknowledgement encrypted with the session key to start the encrypted session.

5. Server and Browser now encrypt all transmitted data with the session key.

 **SSL/TLS and Tomcat**

It is important to note that configuring Tomcat to take advantage of secure sockets is usually only necessary when running it as a stand-alone web server.

And if  running Tomcat primarily as a Servlet/JSP container behind another web server, such as Apache or Microsoft IIS, it is usually necessary to configure the primary web server to handle the SSL connections from users. 

**Certificates**

In order to implement SSL, a web server must have an associated Certificate for each external interface (IP address) that accepts secure connections.Certificate as a "digital driver's license".

1. This "driver's license" is cryptographically signed by its owner, and is therefore extremely difficult for anyone else to forge

2. Certificate is typically purchased from a well-known Certificate Authority (CA) such as VeriSign or Thawte

In many cases, however, authentication is not really a concern. An administrator may simply want to ensure that the data being transmitted and received by the server is private and cannot be snooped by anyone who may be eavesdropping on the connection. Fortunately, Java provides a relatively simple command-line tool, called keytool, which can easily create a "self-signed" Certificate. Self-signed Certificates are simply user generated Certificates which have not been officially registered with any well-known CA, and are therefore not really guaranteed to be authentic at all


**Prepare the Certificate Keystore** 

Tomcat currently operates only on JKS, PKCS11 or PKCS12 format keystores.


**JKS:** 
   
 The JKS format is Java's standard "Java KeyStore" format, and is the format created by the keytool command-line utility. This tool is included in the JDK



**PKCS11/ PKCS12** 


 The PKCS12 format is an internet standard, and can be manipulated via (among other things) OpenSSL and Microsoft's Key-Manager.


To create a new JKS keystore from scratch, containing a single self-signed Certificate, execute the following from a terminal command line:

    $ keytool -genkey -alias tomcat -keyalg RSA

This command will create a new file, in the home directory of the user under which you run it, named ".keystore". 

To specify a different location or filename, add the -keystore parameter, followed by the complete pathname to your keystore file as .

    $ Keytool -genkey -alias tomcat -keyalg RSA  -keystore \path\to\my\dir\<keystore-file-name>

After executing this command, you will first be prompted for

1. keystore password
2. and for general information about this Certificate, such as company, contact name, and so on.

Finally, you will be prompted for the key password, which is the password specifically for this Certificate (as opposed to any other Certificates stored in the same keystore file).


If everything was successful, you now have a keystore file with a Certificate that can be used by your server.


**Edit the Tomcat Configuration File**


Tomcat can use two different implementations of SSL:

1. JSSE implementation provided as part of the Java runtime (since 1.4)

    The Java Secure Socket Extension (JSSE) enables secure Internet communications. It provides a framework and an implementation for a Java version of the SSL and TLS protocols and includes functionality for data encryption, server authentication, message integrity, and optional client authentication

    The JSSE API was designed to allow other SSL/TLS protocol and Public Key Infrastructure (PKI) implementations to be plugged in seamlessly. Developers can also provide alternative logic to determine if remote hosts should be trusted or what authentication key material should be sent to a remote host.

2.  APR implementation, which uses the OpenSSL engine by default.


The exact configuration details of Connector depend on which implementation is being used.

    <!-- Default in configuration file ..-->
    
    <Connector protocol="HTTP/1.1"  port="8080" .../>


To define a Java (JSSE) connector, regardless of whether the APR library is loaded or not, use one of the following:

    <!-- Define a HTTP/1.1 Connector on port 8443, JSSE NIO implementation -->
    
    <Connector protocol="org.apache.coyote.http11.Http11NioProtocol"
               port="8443" .../>

Alternatively, to specify an APR connector (the APR library must be available) use:

    <!-- Define a HTTP/1.1 Connector on port 8443, APR implementation -->
    <Connector protocol="org.apache.coyote.http11.Http11AprProtocol"
               port="8443" .../>


to configure the Connector in the $CATALINA_BASE/conf/server.xml file


    <!-- Define a SSL Coyote HTTP/1.1 Connector on port 8443 -->
    <Connector
               protocol="org.apache.coyote.http11.Http11NioProtocol"
               port="8443" maxThreads="200"
               scheme="https" secure="true" SSLEnabled="true"
               keystoreFile="${user.home}/.keystore" keystorePass="changeit"
               clientAuth="false" sslProtocol="TLS"/>


If you change the port number here, you should also change the value specified for the redirectPort attribute on the non-SSL connector. This allows Tomcat to automatically redirect users who attempt to access a page with a security constraint specifying that SSL is required, as required by the Servlet Specification.

**Configure in web.xml for particular project**

    <security-constraint> 
      <web-resource-collection> 
        <web-resource-name>SUCTR</web-resource-name> 
        <url-pattern>/*</url-pattern>      
      </web-resource-collection> 
      <user-data-constraint> 
        <transport-guarantee>CONFIDENTIAL</transport-guarantee> 
      </user-data-constraint> 
    </security-constraint>


**Installing a Certificate from a Certificate Authority**


1. Create a local Certificate Signing Request (CSR)

1. Create a local self-signed Certificate as described above

2. The CSR is then created with


     $ keytool -certreq -keyalg RSA -alias tomcat -file certreq.csr
        -keystore <your_keystore_filename>

Now you have a file called certreq.csr that you can submit to the Certificate Authority


**Importing the Certificate**

Now that you have your Certificate and you can import it into you local keystore. First of all you have to import a Chain Certificate or Root Certificate into your keystore. After that you can proceed with importing your Certificate.

1. Download a Chain Certificate from the Certificate Authority you obtained the Certificate from

2. Import the Chain Certificate into your keystore
    

     $ keytool -import -alias root -keystore <your_keystore_filename>
        -trustcacerts -file <filename_of_the_chain_certificate>

3. And finally import your new Certificate
   

     keytool -import -alias tomcat -keystore <your_keystore_filename>
        -file <your_certificate_filename>



**Reference :** [Here][1]
    


  [1]: https://tomcat.apache.org/tomcat-7.0-doc/ssl-howto.html#Prepare_the_Certificate_Keystore

