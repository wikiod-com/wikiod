---
title: "Analyzers"
slug: "analyzers"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Analyzers take the text from a string field and generate tokens that will be used when querying.

An Analyzer operates in a sequence:
* `CharFilters` (Zero or more)
* `Tokenizer` (One)
* `TokenFilters` (Zero or more)

The analyzer may be applied to mappings so that when fields are indexed, it is done on a per token basis rather than on the string as a whole. When querying, the input string will also be run through the Analyzer. Therefore, if you normalize text in the Analyzer, it will always match even if the query contains a non-normalized string.

## Analyzers
**Analysis in elasticsearch** comes into context when you are willing to analyze the data in your index.

Analyzers allow us to perform following:

 - Abbreviations
 - Stemming
 - Typo Handling

We will be looking at each of them now.

 1. **Abbreviations**:

    Using analyzers, we can tell elasticsearch how to treat abbreviations in our data i.e. dr => Doctor so whenever we search for doctor keyword in our index, elasticsearch will also return the results which have dr mentioned in them.
 

 2. **Stemming**:
    
    Using stemming in analyzers allows us to use base words for modified verbs like  

    | Word | Modifications |
    | ------ | ------ |
    | require   | requirement,required   |

 

 3. **Typo Handling**:
    
    Analyzers also provide typo handling as while querying if we are searching for particular word say 'resurrection', then elasticsearch will return the results in which typos are present.i.e. it will treat typos like resurection,ressurection as same and will retun the result. 

    | Word | Modifications |
    | ------ | ------ |
    | resurrection   | resurection,ressurection  |

**Analyzers in Elasticsearch**

 1. Standard

 2. Simple

 3. Whitespace

 4. Stop

 5. Keyword

 6. Pattern

 7. Language

 8. Snowball




## Mapping
An Analyzer can be applied to a mapping by using "analyzer", by default the "standard" Analyzer is used.
Alternatively, if you do not wish to have any analyzer used (because tokenizing or normalization would not be useful) you may specify "index":"not_analyzed"

    PUT my_index 
    {
      "mappings": {
        "user": {
          "properties": {
            "name": {
              "type": "string"
              "analyzer":  "my_user_name_analyzer"
            },
            "id": {
              "type": "string",
              "index": "not_analyzed"
            }
          }
        }
      }
    }

## Multi-fields
Sometimes it maybe useful to have multiple distinct indexes of a field with different Analyzers. You can use the multi-fields capability to do so.

    PUT my_index 
    {
      "mappings": {
        "user": {
          "properties": {
            "name": {
              "type": "string"
              "analyzer": "standard",
              "fields": {
                "special": {
                   "type": "string",
                   "analyzer": "my_user_name_analyzer"
                },
                "unanalyzed": {
                  "type": "string",
                  "index": "not_analyzed"
                }
              }
            }
          }
        }
      }
    }

When querying, instead of simply using "user.name" (which in this case would still use the stanard Analyzer) you can use "user.name.special" or "user.name.unanalyzed". Note that the document will remain unchanged, this only affects indexing.

## Ignore case analyzer
Sometimes, we may need to ignore the case of our query, with respect to the match in the document. An analyzer can be used in this case to ignore the case while searching. Each field will have to contain this analyzer in it's property, in order to work:

    "settings": {
            "analysis": {
                "analyzer": {
                    "case_insensitive": {
                   "tokenizer": "keyword",
                   "filter": ["lowercase"]
                }
                }
            }
        }



