---
title: "Connect to couchbase over SSL using SDK"
slug: "connect-to-couchbase-over-ssl-using-sdk"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

In this post I am giving an example on how to connect to Couchbase over SSL/TLS to establish a secure connection to protect data on wire.

Hopefully, you have enabled SSL on Couchbase side. For Information on enabling SSL on Couchbase side you can refer to http://docs.couchbase.com/developer/dotnet-2.0/configuring-ssl.html .

In example, I am setting required cipher suite and enabled protocols.

 

## Secure connection to couchbase using java sdk with specific cipher suites and protocols
    import com.couchbase.client.core.endpoint.SSLEngineFactory
    import com.couchbase.client.java.env.DefaultCouchbaseEnvironment
    import com.couchbase.client.java.CouchbaseCluster
    
    object CouchbaseConnection extends  App {
      
    //Create default environment object. 
    //Set the keystone file path(download keystone from couch base cluster) and keystore password
    
      val  env = DefaultCouchbaseEnvironment
        .builder()
        .sslEnabled(true)
        .sslKeystoreFile("./conf/couchbase.keystore") //
        .sslKeystorePassword("pR8PHe452353546474778r4reThUfu45678523422")
        .build();
    
    //Get all SSL configuration for the default environment
    
     val sslEngineFactory = new SSLEngineFactory(env)
      val sslEngine:SSLEngine = sslEngineFactory.get()
    
    //Set the list of enabled ciphers and transport protocols
      sslEngine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_256_CBC_SHA"))
      sslEngine.setEnabledProtocols(Array("TLSv1.2"))
    
      val cluster = CouchbaseCluster.create(env,"127.0.0.1")
      // Open a bucket person
      val bucket = cluster.openBucket("person","test123")
    }

