---
title: "Retrieve and Rank"
slug: "retrieve-and-rank"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

The Solrj client and the Java SDK are independent so you can update them individually. Always make sure you use the latest version of the Java SDK.

See the GitHub release page for updates https://github.com/watson-developer-cloud/java-sdk/releases

## Search and Rank using the Retrieve and Rank in Java
Install the required dependencies:

    'org.apache.solr:solr-solrj:5.5.1'
    'org.apache.httpcomponents:httpclient:4.3.6'
    'com.ibm.watson.developer_cloud:java-sdk:3.2.0'

The code below assumes you have a Solr collection with documents and you have trained a ranker, otherwise follow this [tutorial][1]


    public class RetrieveAndRankSolrJExample {
    
      private static HttpSolrClient solrClient;
      private static RetrieveAndRank service;
    
      private static String USERNAME = "<username>";
      private static String PASSWORD = "<password>";
      private static String SOLR_CLUSTER_ID = "<your-solr-cluster-id>";
      private static String SOLR_COLLECTION_NAME = "<your-collection-name>";
      private static String RANKER_ID = "<ranker-id>";
      
      public static void main(String[] args) throws SolrServerException, IOException {
         
        // create the retrieve and rank instance
        service = new RetrieveAndRank();
        service.setUsernameAndPassword(USERNAME, PASSWORD);
    
        // create the solr client
        String solrUrl = service.getSolrUrl(SOLR_CLUSTER_ID);
        solrClient = new HttpSolrClient(solrUrl, createHttpClient(solrUrl, USERNAME, PASSWORD));
    
        // build the query
        SolrQuery query = new SolrQuery("*:*");
        query.setRequestHandler("/fcselect");
        query.set("ranker_id", RANKER_ID);
        
        // execute the query
        QueryResponse response = solrClient.query(SOLR_COLLECTION_NAME, query);
        System.out.println("Found " + response.getResults().size() + " documents!");
        System.out.println(response);
      }
    
      private static HttpClient createHttpClient(String uri, String username, String password) {
        final URI scopeUri = URI.create(uri);
    
        final BasicCredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(new AuthScope(scopeUri.getHost(), scopeUri.getPort()),
            new UsernamePasswordCredentials(username, password));
    
        final HttpClientBuilder builder = HttpClientBuilder.create()
            .setMaxConnTotal(128)
            .setMaxConnPerRoute(32)
            .setDefaultRequestConfig(RequestConfig.copy(RequestConfig.DEFAULT).setRedirectsEnabled(true).build())
            .setDefaultCredentialsProvider(credentialsProvider)
            .addInterceptorFirst(new PreemptiveAuthInterceptor());
        return builder.build();
      }
    
      private static class PreemptiveAuthInterceptor implements HttpRequestInterceptor {
        public void process(final HttpRequest request, final HttpContext context) throws HttpException {
          final AuthState authState = (AuthState) context.getAttribute(HttpClientContext.TARGET_AUTH_STATE);
    
          if (authState.getAuthScheme() == null) {
            final CredentialsProvider credsProvider = (CredentialsProvider) context
                .getAttribute(HttpClientContext.CREDS_PROVIDER);
            final HttpHost targetHost = (HttpHost) context.getAttribute(HttpCoreContext.HTTP_TARGET_HOST);
            final Credentials creds = credsProvider.getCredentials(new AuthScope(targetHost.getHostName(),
                targetHost.getPort()));
            if (creds == null) {
              throw new HttpException("No creds provided for preemptive auth.");
            }
            authState.update(new BasicScheme(), creds);
          }
        }
      }
    }


  [1]: https://www.ibm.com/watson/developercloud/doc/retrieve-rank/get_start.shtml

