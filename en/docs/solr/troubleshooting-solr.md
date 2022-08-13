---
title: "Troubleshooting Solr"
slug: "troubleshooting-solr"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

When searching against the target of a copyField instructions, the analyzer stack used is the one of the copyField target, not source. This is especially important to remember with default configuration that searches against generic **text** field.

## Has my content actually indexed?
Often people try to index some content and then find it. If they cannot see the expected result, they try to troubleshoot the whole end-to-end process. The better way is to see whether the content actually indexed in the expected fields. This way it splits the problem into two: indexing and searching.

The easiest way to verify what indexed is in the Admin UI's Schema screen (1). Just select the relevant field (2) and load its Term info (3). That's all the indexed terms in that field. The list (4) could be quite long, but you can change number of items shown and/or index into a separate test field just for debugging. 

[![Example Schema screen in the Admin UI showing Loaded Terms for the "cat" field][1]][1]

If you see nothing or no expected content, it usually means the indexing failed. If you see the content here, but not in the query, then it is the search you need to troubleshoot.


  [1]: http://i.stack.imgur.com/JshM8.png

