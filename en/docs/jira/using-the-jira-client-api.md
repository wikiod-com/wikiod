---
title: "Using the Jira Client API"
slug: "using-the-jira-client-api"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Get Issues From a JQL Query
    final AsynchronousJiraRestClientFactory factory = new AsynchronousJiraRestClientFactory();
    restClient = factory.createWithBasicHttpAuthentication(URI.create(JIRA_SERVER), JIRA_LOGIN, JIRA_PASSWORD);
    getFilteredIssues("project = TestProject");

    public Iterable<Issue> getFilteredIssues(String filterJql) throws Exception {
        final SearchRestClient searchClient = this.connection.getRestClient().getSearchClient();
        HashSet<String> fields = Sets.newHashSet("*all");

        int total = Integer.MAX_VALUE;
        final ArrayList<Issue> issuesList = new ArrayList<>();
        while(issuesList.size() < total){
            Promise<SearchResult> promiseResult = searchClient.searchJql(filterJql, 50, issuesList.size(), fields);
            final SearchResult issues = promiseResult.get();
            issuesList.addAll((Collection<? extends Issue>) issues.getIssues());
        
            if (total == Integer.MAX_VALUE) {
                total = issues.getTotal();
            }
        }        
        
        return issuesList;
        
    }

