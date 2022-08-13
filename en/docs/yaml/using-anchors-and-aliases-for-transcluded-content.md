---
title: "Using anchors and aliases for transcluded content"
slug: "using-anchors-and-aliases-for-transcluded-content"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Creating an "Array of Dictionaries" table with YAML anchors as row identifiers
        ---
        person_table:
          - &person001
            fname:  homer
            lname:  simpson
            role:   dad
            age:    33

          - &person002
            fname:  marge
            lname:  simpson
            role:   mom
            age:    34

          - &person003
            fname:  peter
            lname:  griffin
            role:   dad
            age:    34
            
## Problem

* developer wishes to express a table structure in YAML, where each row is referenced by a compact row identifier

## Solution

* use YAML anchors, by assigning an anchor identifier to each row in the table
* in YAML, reusable "transclusion identifiers" are called anchors and aliases
* in YAML, reusable "transclusion identifiers" consist of alphanumeric tokens preceeded by an ampersand or asterisk

## Rationale

* YAML anchors and aliases allow for increased data normalization
* YAML anchors and aliases enforce `DRY` (Don't repeat yourself)
* in this example, a table structure can be designed and preserved which closely coincides with a database

## Pitfalls

* YAML anchors must be declared before they can be referenced by aliases
* YAML anchors must be unique throughout the document
* failure to specify unique anchors will cause an error on `yaml.load()`
* not all YAML parsers reliably support anchors and aliases

## See also

[Stackoverflow YAML][1]

[1]: http://stackoverflow.com/tags/yaml/info

## Using YAML aliases to cross-reference rows from a YAML table
        ---
        person_table:
          - &person001
            fname:  homer
            lname:  simpson
            role:   dad
            age:    33

          - &person002
            fname:  marge
            lname:  simpson
            role:   mom
            age:    34

          - &person003
            fname:  peter
            lname:  griffin
            role:   dad
            age:    34
            
        motto_table:
          - &motto001
            person:   *person001
            motto: >
              D'oh!! YAML is too complicated!

          - &motto002
            person:   *person002
            motto: >
              Bart! Listen to your father!

          - &motto003
            person:   *person003
            motto: >
              Hey! YAML is freakin' sweet!
            
## Problem

* developer wishes to cross-reference rows-with-anchors from one table and link to them with rows-as-aliases in another table

## Solution

* use YAML aliases, which cross-reference pre-defined anchors from another table
* in YAML, reusable "transclusion identifiers" are called anchors and aliases
* in YAML, reusable "transclusion identifiers" consist of alphanumeric tokens preceeded by an ampersand or asterisk

## Rationale

* YAML anchors and aliases allow for increased data normalization
* YAML anchors and aliases enforce `DRY` (Don't repeat yourself)
* in this example, a table structure can be designed and preserved which closely coincides with a database
* in this example, data entry and file sizes can be reduced 

## Pitfalls

* in this specific example, `yaml.load()` will produce nested dictionaries
    * this is referred to as the "nested dictionaries problem"
    * under the person name-value pair, the value for person will be a sub-dictionary
    * this may be undesirable, because it breaks the uniformity of the table structure
* failure to correctly specify aliases will result in missing data
    * (typos will create broken cross-references)
* YAML does not support file transclusion by reference, so all aliases and anchors must exist in the same yaml file
* not all YAML parsers reliably support anchors and aliases

## See also

[Stackoverflow YAML][1]

[1]: http://stackoverflow.com/tags/yaml/info


## Using YAML merge-keys to cross-reference rows from another YAML table
        ---
        person_table:
          - &person001
            fname:  homer
            lname:  simpson
            role:   dad
            age:    33

          - &person002
            fname:  marge
            lname:  simpson
            role:   mom
            age:    34

          - &person003
            fname:  peter
            lname:  griffin
            role:   dad
            age:    34
            
        motto_table:
          - &motto001
            <<: *person001
            motto: >
              D'oh!! YAML is too complicated!

          - &motto002
            <<: *person002
            motto: >
              Bart! Listen to your father!

          - &motto003
            <<: *person003
            motto: >
              Hey! YAML is freakin' sweet!
            
## Problem

* developer wishes to cross-reference rows-with-anchors from one table and link to them with rows-as-aliases in another table
* developer wishes to avoid creating the "nested dictionaries problem"

## Solution

* use YAML aliases, with YAML merge keys
* in YAML, reusable "transclusion identifiers" are called anchors and aliases
* in YAML, reusable "transclusion identifiers" consist of alphanumeric tokens preceeded by an ampersand or asterisk

## Rationale

* YAML anchors and aliases allow for increased data normalization
* YAML anchors and aliases enforce `DRY` (Don't repeat yourself)
* in this example, a table structure can be designed and preserved which closely coincides with a database
* in this example, data entry and file sizes can be reduced 

## Pitfalls

* in this specific example, `yaml.load()` will produce nested dictionaries
    * under the person name-value pair, the value for person will be a sub-dictionary
    * this may be undesirable, because it breaks the uniformity of the table structure
* failure to correctly specify aliases will result in missing data
    * (typos will create broken cross-references)
* YAML does not support file transclusion by reference, so all aliases and anchors must exist in the same yaml file
* not all YAML parsers reliably support anchors and aliases

## See also

* [Stackoverflow YAML][1]
* [YAML merge-keys specification][2]

[1]: http://stackoverflow.com/tags/yaml/info
[2]: http://yaml.org/type/merge.html



