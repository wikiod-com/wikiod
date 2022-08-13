---
title: "Get shard leaders using SolrJ"
slug: "get-shard-leaders-using-solrj"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

In some cases, for instance when we want to backup solrcloud collections programmatically, we need to know what are the Shard Leaders.
The example below shows how that can be achieved using SolrJ.

## Get shard leaders using SolrJ API
    private final String COLLECTION_NAME = "myCollection";
    private final String ZOOKEPER_CLIENT_TIMEOUT_MS = "1000000"
    
       private Map<String, String> getShardLeaders(CloudSolrServer cloudSolrServer) throws InterruptedException, KeeperException {
            Map<String, String> shardleaders = new TreeMap<String, String>();
            ZkStateReader zkStateReader = cloudSolrServer.getZkStateReader();
            for (Slice slice : zkStateReader.getClusterState().getSlices(COLLECTION_NAME)) {
                shardleaders.put(slice.getName(), zkStateReader.getLeaderUrl(COLLECTION_NAME, slice.getName(), ZOOKEPER_CLIENT_TIMEOUT_MS));
            }
            return shardleaders;
        }

