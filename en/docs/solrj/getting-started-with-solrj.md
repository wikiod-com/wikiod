---
title: "Getting started with solrj"
slug: "getting-started-with-solrj"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
SolrJ comes as part of the Solr distribution since Solr 1.x. The latest Solr version can be downloaded [here][1].
From Solr-6.3.0 that is the latest version available, we need to get the following libraries and add them to our build path:
/dist/apache-solr-solrj-*.jar
/dist/solrj-lib/*

Once we're done, SolrJ is ready to communicate with our Solr instance. In the following example we'll see how to use SolrJ to query or add content to Solr.

  [1]: http://www.apache.org/dyn/closer.lua/lucene/solr/6.3.0

## Add documents to Solr using SolrJ
If we have a schema looking like:

    <field name="id" type="string" indexed="true" stored="true" required="true" /> 
    
    <field name="name" type="string" indexed="true" stored="true" />

the solrj code to add content to Solr will look like: 

 

     package com.stackoverflow.solrj.example;
    
    import java.io.IOException;
    import java.util.ArrayList;
    import java.util.Collection;
    
    import org.apache.solr.client.solrj.SolrClient;
    import org.apache.solr.client.solrj.SolrServerException;
    import org.apache.solr.client.solrj.impl.HttpSolrClient;
    import org.apache.solr.common.SolrInputDocument;
    
    public class SolrJIndexing {
    
        private final static String SOLR_URL = "http://localhost:8983/solr/mycollection";
    
        public static void main(String[] args) {
            SolrClient solrClient = new HttpSolrClient.Builder(SOLR_URL).build();
            SolrInputDocument document1 = getDocument(1, "Document example 1");
            SolrInputDocument document2 = getDocument(2, "Document example 2");
            Collection<SolrInputDocument> docs = new ArrayList<SolrInputDocument>();
            docs.add(document1);
            docs.add(document2);
            try {
                solrClient.add(docs);
                solrClient.commit();
            } catch (SolrServerException | IOException e) {
                e.printStackTrace();
            }
        }
    
        private static SolrInputDocument getDocument(int id, String name) {
            SolrInputDocument document = new SolrInputDocument();
            document.addField("id", id);
            document.addField("name", name);
            return document;
        }
    
    }

## Query Solr with SolrJ
If you added successfully documents to Solr using the previous example, you'll be now able to retrieve them in this way:

    package com.stackoverflow.solrj.example;
    
    import java.io.IOException;
    import java.util.Iterator;
    
    import org.apache.solr.client.solrj.SolrClient;
    import org.apache.solr.client.solrj.SolrQuery;
    import org.apache.solr.client.solrj.SolrServerException;
    import org.apache.solr.client.solrj.impl.HttpSolrClient;
    import org.apache.solr.client.solrj.response.QueryResponse;
    import org.apache.solr.common.SolrDocument;
    import org.apache.solr.common.SolrDocumentList;
    
    public class SolrJQueryExample {
    
        private final static String SOLR_URL = "http://localhost:8983/solr/mycollection";
    
        public static void main(String[] args) throws SolrServerException, IOException {
    
            SolrClient solrClient = new HttpSolrClient.Builder(SOLR_URL).build();
            SolrQuery solrQuery = new SolrQuery();
            solrQuery.setQuery("example");
            solrQuery.setStart(0);
            solrQuery.setRows(10);
    
            QueryResponse queryResponse = solrClient.query(solrQuery);
            SolrDocumentList solrDocs = queryResponse.getResults();
            Iterator<SolrDocument> iterator = solrDocs.iterator();
            while (iterator.hasNext()) {
                SolrDocument solrDocument = iterator.next();
                String docId = (String) solrDocument.getFieldValue("id");
                String docName = (String) solrDocument.getFieldValue("name");
                System.out.println("Document " + docId + ": " + docName);
            }
    
        }
    }

## Init SolrJ client for local STANDALONE cluster vs remote CLOUD with Kerberos
Handy method to initialise SolrJ client based on configuration properties;

    private void initSolrClient() {
        if (conf.kerberos().isEnabled()) {
            System.setProperty("java.security.auth.login.config", conf.kerberos().confPath());
            HttpClientUtil.addConfigurer(new Krb5HttpClientConfigurer());
        }
        if (conf.solr().getMode() == SolrProperties.Mode.STANDALONE) {
            this.solr = new HttpSolrClient.Builder(conf.solr().url()).build();
        }
        if (conf.solr().getMode() == SolrProperties.Mode.CLOUD) {
            this.solr = new CloudSolrClient.Builder().withZkHost(conf.solr().zookeepers()).build();
        }
    }

REMOTE with Kerberos:

    solr.conf.mode=CLOUD
    solr.conf.zookepers=zookeeper-01.mydomain.com:2181,zookeeper-02.mydomain.com:2181,zookeeper-03.mydomain.com:2181/solr
    kerberos.enabled=true
    kerberos.confPath=/my/path/to/krb5/jaas-client.conf

LOCAL development:

    solr.conf.mode=STANDALONE
    solr.conf.url=http://localhost:8983/solr
    kerberos.enabled=false



