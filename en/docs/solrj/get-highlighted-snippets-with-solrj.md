---
title: "Get highlighted snippets with SolrJ"
slug: "get-highlighted-snippets-with-solrj"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

When we get highlighted snippets in our Solr response programmatically, we need to extract and handle them. The code below shows how to do that with SolrJ.

## Get highlighted text for single-valued fields
    public String getHighlightedText(final QueryResponse queryResponse, final String fieldName, final String docId) {
        String highlightedText = "";
        Map<String, Map<String, List<String>>> highlights = queryResponse.getHighlighting();
        if (highlights!=null && MapUtils.isNotEmpty(highlights.get(docId))) {
            List<String> snippets = highlights.get(docId).get(fieldName);
            if (CollectionUtils.isNotEmpty(snippets)) {
                highlightedText = getFragments(snippets);
            }
        }
        return highlightedText;
    }
    
    private static final String getFragments(List<String> snippets){
            StringBuilder fragments = new StringBuilder();
            for (int i = 0; i < snippets.size(); i++) {
                if (i > 0) {
                    fragments.append("............");
                }
                fragments.append(snippets.get(i));
            }
            return fragments.toString();
        }

